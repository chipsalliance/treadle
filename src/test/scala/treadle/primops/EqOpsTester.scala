// See LICENSE for license details.

package treadle.primops

import firrtl.ExecutionOptionsManager
import firrtl.ir
import firrtl.ir.Type
import org.scalatest.{FreeSpec, Matchers}
import treadle.{BlackBoxFactory, BlackBoxImplementation, TreadleTester, HasTreadleSuite}

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

class BlackBoxTypeParamFactory extends BlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName : String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "BlackBoxTypeParam" => Some(add(new BlackBoxTypeParam_1(instanceName)))
    }
  }
}

// scalastyle:off magic.number
class EqOpsTester extends FreeSpec with Matchers {
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

    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--no-default-reset",
            "--blackbox-factory", "treadle.primops.BlackBoxTypeParamFactory",
            "--show-firrtl-at-load",
            "--firrtl-source", input)) with HasTreadleSuite
    val tester = TreadleTester(optionsManager)

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

    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--show-firrtl-at-load",
            "--fint-rollback-buffers", "0",
            "--firrtl-source", input)) with HasTreadleSuite
    val tester = new TreadleTester(optionsManager)
    tester.peek("out") should be (BigInt(0))
    println(s"peek out ${tester.peek("out") != BigInt(0)}")
    tester.report()
  }
}
