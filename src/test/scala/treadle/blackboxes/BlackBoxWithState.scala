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

package treadle.blackboxes

import firrtl.ir.Type
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable._
import treadle._

// scalastyle:off magic.number
/**
  * Demonstrates how a black box can maintain and change internal state
  * based on clock transitions
  */
class BlackBoxWithState extends FreeSpec with Matchers {
  "BlackBoxWithState should pass a basic test" in {
    val input =
      """
        |circuit AccumBlackBoxWrapper : @[:@2.0]
        |  extmodule AccumBlackBox : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@4.4]
        |    output data : UInt<16> @[:@5.4]
        |
        |    defname = AccumBlackBox
        |
        |
        |  module AccumBlackBoxWrapper : @[:@10.2]
        |    input clock : Clock @[:@11.4]
        |    input reset : UInt<1> @[:@12.4]
        |    output io_data : UInt<16> @[:@13.4]
        |
        |    inst m of AccumBlackBox @[AccumBlackBoxSpec.scala 93:17:@15.4]
        |    node _T_4 = bits(reset, 0, 0) @[AccumBlackBoxSpec.scala 96:9:@20.4]
        |    node _T_6 = eq(_T_4, UInt<1>("h0")) @[AccumBlackBoxSpec.scala 96:9:@21.4]
        |    io_data <= m.data
        |    m.clock <= clock
        |    m.reset <= reset
        |
      """.stripMargin

    val options = Seq(
      SymbolsToWatchAnnotation(Seq("io_data")),
      BlackBoxFactoriesAnnotation(Seq(new AccumBlackBoxFactory)),
      ClockInfoAnnotation(Seq(ClockInfo("clock", 10, 8))),
      CallResetAtStartupAnnotation
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    val initialValue = tester.peek("io_data")
    println(s"Initial value is $initialValue")
    tester.step()
    tester.expect("io_data", initialValue + 1)
    println(s"m.data ${tester.peek("m.data")}")
    tester.step()
    tester.expect("io_data", initialValue + 2)
    println(s"m.data ${tester.peek("m.data")}")

    tester.report()
  }
}

/**
  * This is an implementation of a black box whose verilog is contained inline in AccumBlackBox, an instance of this
  * class will be placed into a black box factory so that it can be passed properly to the firrtl engine
  * @param name black box name
  */
class AccumulatorBlackBox(val name: String) extends ScalaBlackBox {

  var ns:        BigInt = 0
  var ps:        BigInt = 0
  var isInReset: Boolean = false

  override def inputChanged(name: String, value: BigInt): Unit = {}

  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "data" => Seq("clock", "reset")
      case _      => Seq.empty
    }
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    transition match {
      case PositiveEdge =>
        if (!isInReset) {
          ps = ns
        }
        ns = ps + 1
      // println(s"blackbox:$name ps $ps ns $ns")
      case _ =>
      // println(s"not positive edge, no action for cycle in $name")
    }
  }

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    isInReset = inputValues.last != BigInt(0)
    ps
  }
}

/**
  * The factor that will provide firrtl access to the implementations
  */
class AccumBlackBoxFactory extends ScalaBlackBoxFactory {

  def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "AccumBlackBox" => Some(add(new AccumulatorBlackBox(instanceName)))
      case _               => None
    }
  }
}
