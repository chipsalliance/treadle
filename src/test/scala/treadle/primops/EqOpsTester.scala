/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.primops

import firrtl.ir
import firrtl.ir.Type
import firrtl.stage.FirrtlSourceAnnotation
import treadle._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BlackBoxTypeParam_1(val name: String) extends ScalaBlackBox {
  var returnValue: BigInt = 0

  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    returnValue
  }

  override def setParams(params: Seq[ir.Param]): Unit = {
    val p1 = params.find(_.name == "T")
    p1.foreach {
      case valueParam: firrtl.ir.RawStringParam =>
        returnValue = BigInt("deadbeef", 16)
      case _ =>
        println(s"huh?")
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = Seq()
}

// scalastyle:off magic.number
class EqOpsTester extends AnyFreeSpec with Matchers {
  private val factory = new ScalaBlackBoxFactory {
    override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
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

    val options = Seq(
      CallResetAtStartupAnnotation,
      BlackBoxFactoriesAnnotation(Seq(factory))
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    tester.peek("out") should be(1)
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

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
    tester.peek("out") should be(BigInt(0))
    println(s"peek out ${tester.peek("out") != BigInt(0)}")
    tester.report()
  }
}
