// See LICENSE for license details.
//
package treadle

import java.io.File

import firrtl.ExecutionOptionsManager
import treadle.vcd.VCD
import logger.LazyLogging
import treadle.stage.TreadleOptions
import treadle.utils.VcdRunner

/**
  * This tester runs a VCD file against a circuit expressed in a firrtl file.  The VCD file should
  * have been produced by running a test harness against the circuit.  This test can be used to
  * generate circuit behavior while running symbolic or concolic testing.
  * It can also be used to determine if later changes to a circuit have changed since some original
  * correct **golden** run of the circuit
  * For example use the main below to run the VcdAdder files contained in the src/test/resources directory
  * {{{
  * sbt 'runMain treadle.VcdReplayTester -fs src/test/resources/VcdAdder.fir -vcd src/test/resources/VcdAdder.vcd'
  * }}}
  *
  * @param optionsManager Used to set various options
  */
class VcdReplayTester(optionsManager: VcdReplayTesterOptions) extends LazyLogging {

  private def getInput(fileName: String): String = {
    var file = new File(fileName)
    if(! file.exists()) {
      file = new File(fileName + ".fir")
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    io.Source.fromFile(file).mkString
  }

  val vcdTesterOptions: VcdReplayOptions = optionsManager.goldenVcdOptions
  val treadleOptions  : TreadleOptions   = optionsManager.treadleOptions

  val tester: TreadleTester = TreadleTester(getInput(vcdTesterOptions.firrtlSourceName), optionsManager)

  val vcd: VCD = VCD.read(vcdTesterOptions.vcdSourceName, tester.engine.ast.main)

  val vcdRunner: VcdRunner = new VcdRunner(tester, vcd)

  def testSuccesses: Long = vcdRunner.testSuccesses
  def testFailures: Long = vcdRunner.testFailures

  def run(): Unit = {
    vcdRunner.setInitialValues()

    val start = vcdTesterOptions.skipEvents
    val end = if(vcdTesterOptions.eventsToRun > 0) start + vcdTesterOptions.eventsToRun else vcdRunner.events.length

    vcdRunner.setNextEvent(start)

    val startTime = System.currentTimeMillis()
    while(vcdRunner.nextEvent < end) {
      println(vcdRunner.eventSummary(vcdRunner.nextEvent))

      vcdRunner.executeNextEvent()

      vcdRunner.testWires(
        vcdRunner.previousEvent,
        justOutputs = false,
        clearResult = false
      )
    }
    val endTime = System.currentTimeMillis()

    tester.finish

    println(f"events run:       ${vcdRunner.eventsRun}%10d")
    println(f"input values set: ${vcdRunner.inputValuesSet}%10d")
    println(f"values tested:    ${vcdRunner.valuesTested}%10d")
    println(f"test successes:   ${vcdRunner.testSuccesses}%10d")
    println(f"test failures:    ${vcdRunner.testFailures}%10d")
    println(f"clock cycles:     ${tester.cycleCount}%10d")
    println(f"                  ${tester.cycleCount / ((endTime - startTime) / 1000.0)}%10.2f Hz")
    println(f"run time:         ${(endTime - startTime) / 1000.0}%10.2f seconds")
  }
}

object VcdReplayTester {
  def main(args: Array[String]) {
    val optionsManager = new VcdReplayTesterOptions

    if (optionsManager.parse(args)) {
      val repl = new VcdReplayTester(optionsManager)
      repl.run()
    }
  }
}

case class VcdReplayOptions(
    firrtlSourceName:     String = "",
    vcdSourceName:        String = "",
    skipEvents:           Int = 0,
    eventsToRun:          Int = -1,
    testAliasedWires:     Boolean = false)
  extends firrtl.ComposableOptions

trait HasVcdReplayOptions {
  self: ExecutionOptionsManager =>

  var goldenVcdOptions = VcdReplayOptions()

  parser.note("golden-vcd")

  parser.opt[String]("firrtl-source")
    .abbr("fs")
    .valueName("<firrtl-source-file>")
    .foreach { x => goldenVcdOptions = goldenVcdOptions.copy(firrtlSourceName = x) }
    .text("firrtl source file to load on startup")

  parser.opt[String]("vcd-file")
    .abbr("vcd")
    .valueName("<vcd-file>")
    .foreach { x => goldenVcdOptions = goldenVcdOptions.copy(vcdSourceName = x) }
    .text("firrtl source file to load on startup")

  parser.opt[Int]("skip-events")
    .abbr("se")
    .valueName("<number>")
    .foreach { x => goldenVcdOptions = goldenVcdOptions.copy(skipEvents = x) }
    .text("number of events to skip before starting")

  parser.opt[Int]("events-to-run")
    .abbr("etr")
    .valueName("<number>")
    .foreach { x => goldenVcdOptions = goldenVcdOptions.copy(eventsToRun = x) }
    .text("number of events to run")

  parser.opt[Unit]("test-aliased-wires")
    .abbr("taw")
    .foreach { _ => goldenVcdOptions = goldenVcdOptions.copy(testAliasedWires = true) }
    .text("number of events to run")
}

class VcdReplayTesterOptions extends TreadleOptionsManager with HasVcdReplayOptions

