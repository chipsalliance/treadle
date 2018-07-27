// See LICENSE for license details.
//
package treadle

import java.io.File

import firrtl.annotations.NoTargetAnnotation
import firrtl.{AnnotationSeq, HasFirrtlExecutionOptions}
import treadle.vcd.VCD
import logger.LazyLogging
import treadle.utils.VcdRunner
import scopt.OptionParser
import firrtl.options._
import firrtl.options.Viewer._
import treadle.VcdReplayOptionsViewer._
import treadle.TreadleViewer._

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
  * @param annotationSeq all the annotations
  */
class VcdReplayTester(annotationSeq: AnnotationSeq) extends LazyLogging {

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

  val vcdTesterOptions: VcdReplayExecutionOptions = view[VcdReplayExecutionOptions](annotationSeq).get
  val treadleOptions  : TreadleExecutionOptions   = view[TreadleExecutionOptions](annotationSeq).get

  val tester: TreadleTester = new TreadleTester(getInput(vcdTesterOptions.firrtlSourceName), annotationSeq)

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

case object VcdReplayExecutionResult extends DriverExecutionResult

object VcdReplayTester extends firrtl.options.Driver {
  val optionsManager: ExecutionOptionsManager = {
    new ExecutionOptionsManager("vcd-replay") with HasFirrtlExecutionOptions
  }

  override def execute(args: Array[String], initialAnnotations: AnnotationSeq = Seq.empty): DriverExecutionResult = {
    val annotations = optionsManager.parse(args, initialAnnotations)

    val replayer = new VcdReplayTester(annotations)
    replayer.run()
    ReplExecutionResult
  }
}
