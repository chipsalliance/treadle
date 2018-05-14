// See LICENSE for license details.
//
package treadle

import java.io.File

import firrtl.ExecutionOptionsManager
import treadle.vcd.{VCD, Wire}
import logger.LazyLogging

/**
  * This tester runs a VCD file against a circuit expressed in a firrtl file.  The VCD file should
  * have been produced by running a test harness against the circuit.  This test can be used to
  * generate circuit behavior while running symbolic or concolic testing.
  * It can also be used to determine if later changes to a circuit have changed since some original
  * correct **golden** run of the circuit
  * For example use the main below to run the VcdAdder files contained in the src/test/resources directory
  * {{{
  * sbt 'run-main treadle.VcdReplayTester -fs src/test/resources/VcdAdder.fir \
  * -vcd src/test/resources/VcdAdder.vcd'
  * }}}
  *
  * @param optionsManager Used to set various options
  */
class VcdReplayTester(
    optionsManager: VcdReplayTesterOptions)
  extends LazyLogging {

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

  val vcdTesterOptions = optionsManager.goldenVcdOptions
  val treadleOptions = optionsManager.treadleOptions

  val tester = new TreadleTester(getInput(vcdTesterOptions.firrtlSourceName), optionsManager)
  val engine = tester.engine

  val dutName = engine.ast.main

  val vcd: VCD = VCD.read(vcdTesterOptions.vcdSourceName, dutName)
  val timeStamps = vcd.valuesAtTime.keys.toList.sorted.toArray
  var runVerbose = false

  private var eventsRun = 0
  private var inputValuesSet = 0L
  private var valuesTested = 0L
  private var testSuccesses = 0L
  private var testFailures = 0L
  private var clockCycles = 0L

  val inputs = tester.engine.symbolTable.inputPortsNames

  def hasName(symbolName: String): Boolean = {
    engine.symbolTable.contains(symbolName)
  }

  def setValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName
    if (hasName(fullName)) {
      tester.poke(fullName, newValue)
      println(s"$fullName <= ${engine.symbolTable(fullName).normalize(newValue)}")
      inputValuesSet += 1
    }
  }

  def checkValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName

    valuesTested += 1

    if (hasName(fullName)) {
      val circuitValue = engine.getValue(fullName)

      val result = if(tester.peek(fullName) == newValue) {
        testSuccesses += 1
        "ok"
      }
      else {
        testFailures += 1
        Console.RED + "bad" + Console.RESET
      }
      println(s"Testing $fullName: circuit $circuitValue, vcd $newValue $result")
    }
  }

  def setInitialValues(): Unit = {
    vcd.initialValues.foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        setValue(wire, change.value)
      }
    }
  }

  /**
    * poke the top level inputs
    * @param timeIndex current time from VCS file.
    */
  def setInputs(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = change.wire.fullName
        if (inputs.contains(fullName)) {
          val inputSymbol = engine.symbolTable(fullName)
          if(tester.clockStepper.clockSymbols.contains(inputSymbol)) {
            tester.clockStepper.run(1)
          }
          else {
            setValue(wire, change.value)
          }
        }
      }
    }
  }

  def testWires(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      if (vcdTesterOptions.testAliasedWires) {
        vcd.wiresFor(change).foreach { wire =>
          val fullName = change.wire.fullName
          if (!(inputs.contains(fullName))) {
            checkValue(wire, change.value)
          }
        }
      }
      else {
        val fullName = change.wire.fullName
        if (!(inputs.contains(fullName))) {
          checkValue(change.wire, change.value)
        }
      }
    }
  }

  def checkClock(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).exists { wire =>
        val fullName = change.wire.fullName

        true
      }
    }
  }

  def run(): Unit = {
    println(s"Initial values:\n${vcd.initialValues.mkString("\n")}")
    setInitialValues()
    val start = vcdTesterOptions.skipEvents
    val end = if(vcdTesterOptions.eventsToRun > 0) start + vcdTesterOptions.eventsToRun else timeStamps.length

    val startTime = System.currentTimeMillis()
    for(timeIndex <- start until end) {
      eventsRun += 1
      println(s"Time[$timeIndex]: ${timeStamps(timeIndex)}")

      if(runVerbose) println(s"${vcd.valuesAtTime(timeStamps(timeIndex)).mkString("\n")}")

      setInputs(timeIndex)
      testWires(timeIndex)
    }
    val endTime = System.currentTimeMillis()


    println(f"events run:       $eventsRun%10d")
    println(f"input values set: $inputValuesSet%10d")
    println(f"values tested:    $valuesTested%10d")
    println(f"test successes:   $testSuccesses%10d")
    println(f"test failures:    $testFailures%10d")
    println(f"clock cycles:     $clockCycles%10d")
    println(f"                  ${clockCycles / ((endTime - startTime) / 1000.0)}%10.2f Hz")
    println(f"run time:         ${(endTime - startTime) / 1000.0}%10.2f seconds")
  }
}

object VcdReplayTester {
  def main(args: Array[String]) {
    val optionsManager = new VcdReplayTesterOptions

    optionsManager.parse(args) match {
      case true =>
        val repl = new VcdReplayTester(optionsManager)
        repl.run()
      case _ =>
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

