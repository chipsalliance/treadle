// See LICENSE for license details.

package treadle.primops

import firrtl.ir
import firrtl.ir.Type
import org.scalatest.{FreeSpec, Matchers}
import treadle.{BlackBoxFactory, BlackBoxImplementation, InterpreterOptionsManager, TreadleTester}

class BlackBoxTypeParam_1(val name: String) extends BlackBoxImplementation {
  var returnValue: BigInt = 0

  override def execute(inputValues: Seq[BigInt], tpe: Type, outputName : String): BigInt = {
    returnValue
  }

  override def setParams(params: Seq[ir.Param]): Unit = {
    val p1 = params.find(_.name == "T")
    p1.foreach {
      case valueParam : firrtl.ir.RawStringParam =>
        returnValue = BigInt("deadbeef", 16)
      case _ =>
        println(s"huh?")
    }
  }

  override def outputDependencies(
    outputName: String): Seq[String] = Seq()
}

// scalastyle:off magic.number
class EqOpsTester extends FreeSpec with Matchers {
  private val factory = new BlackBoxFactory {
    override def createInstance(instanceName: String, blackBoxName : String): Option[BlackBoxImplementation] = {
      blackBoxName match {
        case "BlackBoxTypeParam" => Some(add(new BlackBoxTypeParam_1(instanceName)))
      }
    }
  }
  "EqOpsTester should pass a basic test" in {
    val input =
      """
        |circuit EqTester :
        |  extmodule BlackBoxTypeParam_1 :
        |    output out : UInt<32>
        |
        |    defname = BlackBoxTypeParam
        |    parameter T = 'bit [31:0]'
        |
        |  module EqTester :
        |    output out : UInt<1>
        |
        |    inst blackBoxTypeParamWord of BlackBoxTypeParam_1
        |
        |    out <= eq(blackBoxTypeParamWord.out, UInt<32>("hdeadbeef"))
      """.stripMargin

    val optionsManager = new InterpreterOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = true,
        noDefaultReset = true,
        showFirrtlAtLoad = true,
        blackBoxFactories = Seq(factory)
      )
    }
    val tester = TreadleTester(input, optionsManager)

    tester.peek("out") should be (1)
  }
}
