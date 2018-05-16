// See LICENSE for license details.

package treadle.primops

import firrtl.ir
import firrtl.ir.Type
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.{BlackBoxFactory, BlackBoxImplementation}
import treadle.{TreadleOptionsManager, TreadleTester}

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

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        callResetAtStartUp = true,
        showFirrtlAtLoad = false,
        blackBoxFactories = Seq(factory)
      )
    }
    val tester = TreadleTester(input, optionsManager)

    tester.peek("out") should be (1)
  }


  "results of equals on large numbers should have widths all work" in {
    val input =
      """
        |circuit CatProblem :
        |  module CatProblem :
        |    input clock : Clock
        |    output out : UInt<1>
        |
        |    node _T_310 = cat(UInt<32>("hffdff06f"), UInt<32>("h73")) @[Cat.scala 30:58]
        |    node _T_311 = cat(UInt<32>("h0"), UInt<32>("habcdef")) @[Cat.scala 30:58]
        |    node _T_312 = eq(_T_311, _T_310) @[Cat.scala 30:58]
        |
        |    out <= _T_312
        |
          """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = false,
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        rollbackBuffers = 0,
        symbolsToWatch = Seq()
      )
    }

    val tester = new TreadleTester(input, optionsManager)
    tester.peek("out") should be (BigInt(0))
    println(s"peek out ${tester.peek("out") != BigInt(0)}")
    tester.report()
  }
}
