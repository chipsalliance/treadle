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

package treadle.real

import firrtl.stage.FirrtlSourceAnnotation
import treadle._
import treadle.asyncreset.AsyncResetBlackBoxFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BlackBoxRealSpec extends AnyFreeSpec with Matchers {
  "this tests black box implmentation of real numbers" - {
    val adderInput =
      """
        |circuit RealAdder :
        |  extmodule BBFAdd :
        |    output out : UInt<64>
        |    input in2 : UInt<64>
        |    input in1 : UInt<64>
        |
        |
        |  module RealAdder :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input io_a1_node : UInt<64>
        |    input io_a2_node : UInt<64>
        |    output io_c_node : UInt<64>
        |
        |    reg register1_node : UInt<64>, clk with :
        |      reset => (UInt<1>("h0"), register1_node)
        |    inst BBFAdd_1 of BBFAdd @[DspReal.scala 82:36]
        |    wire T_15_node : UInt<64> @[DspReal.scala 67:19]
        |    io_c_node <= register1_node
        |    register1_node <= T_15_node
        |    BBFAdd_1.in2 <= io_a2_node
        |    BBFAdd_1.in1 <= io_a1_node
        |    T_15_node <= BBFAdd_1.out
      """.stripMargin

    "addition should work expand instances as found" in {

      val options = Seq(
        RandomSeedAnnotation(),
        BlackBoxFactoriesAnnotation(Seq(new DspRealFactory))
      )

      val tester = TreadleTester(FirrtlSourceAnnotation(adderInput) +: options)

      tester.poke("io_a1_node", doubleToBigIntBits(1.5))
      tester.poke("io_a2_node", doubleToBigIntBits(3.25))
      tester.step()

      tester.expect("io_c_node", doubleToBigIntBits(4.75))
    }
  }

  "truncation is supported with IntPart" in {
    val input =
      """
        |circuit Trunc :
        |  extmodule BBFIntPart :
        |    output out : UInt<64>
        |    input in : UInt<64>
        |
        |  module Trunc :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input io_a_node : UInt<64>
        |    output io_c_node : UInt<64>
        |
        |    inst BBFIntPart_1 of BBFIntPart @[DspReal.scala 82:36]
        |    io_c_node <= BBFIntPart_1.out
        |    BBFIntPart_1.in <= io_a_node
      """.stripMargin

    val options = Seq(
      RandomSeedAnnotation(),
      BlackBoxFactoriesAnnotation(Seq(new DspRealFactory)),
      RandomSeedAnnotation(0L)
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    tester.poke("io_a_node", doubleToBigIntBits(3.14159))

    tester.expect("io_c_node", doubleToBigIntBits(3.0))
  }
}
