// SPDX-License-Identifier: Apache-2.0

package treadle.blackboxes

import firrtl.ir.Type
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle._
import treadle.executable._

//scalastyle:off magic.number

/**
  * Illustrate a black box that has multiple outputs
  * This one creates 3 outputs each with a different increment of the input
  */
class HasCustomFinish extends ScalaBlackBox {
  override def name: String = "HasCustomFinish"
  var myFactoryOpt: Option[HasCustomFinishFactory] = None

  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    BigInt(42) // always returns 42
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if(transition == PositiveEdge) {
      myFactoryOpt.get.clockUpCount += 1
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  override def finish(): Unit = {
    println(s"HasCustomFinish was saw ${myFactoryOpt.get.clockUpCount} clock cycles")
  }
}

class HasCustomFinishFactory extends ScalaBlackBoxFactory {
  var clockUpCount: Int = 0

  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    val newBlackBox = new HasCustomFinish
    newBlackBox.myFactoryOpt = Some(this)
    Some(add(newBlackBox))
  }
}

class BlackBoxFinishSpec extends AnyFreeSpec with Matchers {
  "this tests black box implementation that have multiple outputs" - {
    val adderInput =
      """
        |circuit CustomFinishTest :
        |  extmodule HasCustomFinish :
        |    input clock : Clock
        |    output out  : UInt<64>
        |
        |  module CustomFinishTest :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out  : UInt<64>
        |
        |    inst fo of HasCustomFinish
        |    fo.clock <= clock
        |    out  <= fo.out
      """.stripMargin

    "each output should hold a different values" in {

      val customFinishFactory = new HasCustomFinishFactory
      val options = Seq(
        BlackBoxFactoriesAnnotation(Seq(customFinishFactory)),
        RandomSeedAnnotation(1L)
      )

      val tester = TreadleTester(FirrtlSourceAnnotation(adderInput) +: options)

      for (i <- 0 until 10) {
        tester.expect("out", BigInt(42))
        tester.step()
      }

      tester.finish

      customFinishFactory.clockUpCount should be >= 10
    }
  }
}
