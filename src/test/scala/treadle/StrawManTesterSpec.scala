// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{Phase, Shell, Stage}
import firrtl.stage.{FirrtlCli, FirrtlSourceAnnotation}
import logger.{Logger, LoggerCli}
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.TreadleException
import treadle.stage.{TreadleCli, TreadleStage, TreadleTesterAnnotation}

/*
 * The goal of this is to provide a straw man construction of
 * how to run a unit test with the TreadleStage pattern
 *
 */

case class StrawManTesterResult(
  cycles: Long,
  runTime: Long,
  successfulTests: Int,
  message: String = ""
) extends NoTargetAnnotation {
  override def toString: String = {
    s"cycles $cycles ms $runTime passed $successfulTests $message"
  }
}

trait StrawManTester extends Phase {
  def transform(a: AnnotationSeq): AnnotationSeq = {
    a.flatMap {
      case t @ TreadleTesterAnnotation(tester) =>
        Seq(t, runner(tester))
      case other =>
        Seq(other)
    }
  }

  /**
    * Here is where a user defines a test
    * @param tester the test harness implementation
    * @return
    */
  def execute(tester: TreadleTester): Unit

  def runner(tester: TreadleTester): StrawManTesterResult = {
    try {
      val startTime = System.currentTimeMillis()
      execute(tester)
      val stopTime = System.currentTimeMillis()
      StrawManTesterResult(
        tester.cycleCount,
        stopTime - startTime,
        tester.expectationsMet
      )
    }
    catch {
      case t: TreadleException =>
        throw t

    }
  }
}

object StrawManTesterStage extends Stage {
  val shell: Shell = new Shell("chisel") with TreadleCli with LoggerCli with FirrtlCli

  private val phases: Seq[Phase] = Seq(
    StrawManAddTester
  )

  def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      phases.foldLeft(annotations) { (annos, phase) => phase.transform(annos) }
    }
  }
}

object StrawManAddTester extends StrawManTester {
  def execute(t: TreadleTester): Unit = {
    t.poke("io_a", 1)
    t.poke("io_b", 1)
    t.step()
    t.expect("io_out", 2)
  }
}


/**
  * Compose two stages and look for the result in the final
  * annotations
  */
class StrawManAdderSpec extends FreeSpec with Matchers {
  "Example of how to create a internal testers pipeline" in {

    val firrtlText = FirrtlSourceAnnotation("""
                       |circuit Add :
                       |  module Add :
                       |    input clock : Clock
                       |    input io_a : UInt<8>
                       |    input io_b : UInt<8>
                       |    output io_out : UInt<9>
                       |
                       |    io_out <= add(io_a, io_b)
                     """.stripMargin)

    val stages = Seq(TreadleStage, StrawManTesterStage)

    val finalAnnotations = stages.foldLeft(AnnotationSeq(Seq(firrtlText))) { (annotations, stage) =>
      stage.run(annotations)
    }

    finalAnnotations.collectFirst { case a: StrawManTesterResult => a } match {
      case Some(result) =>
        println(result)
      case _ =>
        println(s"Test failed")
    }

  }
}
