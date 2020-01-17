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

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

class CarryOrChain3 extends FreeSpec with Matchers {
  private val input =
    """
      |circuit ORChain :
      |  module ORBlock :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a : UInt<1>
      |    input io_ci : UInt<1>
      |    output io_co : UInt<1>
      |
      |    node T_3 = or(io_a, io_ci) @[CarryChainSpecs.scala 19:17]
      |    io_co <= T_3
      |
      |  module ORChain :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a_0 : UInt<1>
      |    input io_a_1 : UInt<1>
      |    input io_a_2 : UInt<1>
      |    output io_co_0 : UInt<1>
      |    output io_co_1 : UInt<1>
      |    output io_co_2 : UInt<1>
      |
      |    inst ORBlock_2 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    inst ORBlock_1_1 of ORBlock @[CarryChainSpecs.scala 35:19]
      |    io_co_0 <= io_a_0
      |    io_co_1 <= ORBlock_2.io_co
      |    io_co_2 <= ORBlock_1_1.io_co
      |    ORBlock_2.io_a <= io_a_1
      |    ORBlock_2.io_ci <= io_co_0
      |    ORBlock_2.clk <= clk
      |    ORBlock_2.reset <= reset
      |    ORBlock_1_1.io_a <= io_a_2
      |    ORBlock_1_1.io_ci <= io_co_1
      |    ORBlock_1_1.clk <= clk
      |    ORBlock_1_1.reset <= reset
    """.stripMargin

  "Carry or chain should work properly" in {
    val N = 3

    def v(bin: String): Array[BigInt] = {
      bin.toList.map("01".indexOf(_)).map(BigInt(_)).reverse.toArray
    }

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    val lst = List((v("001"), v("111")))
    for ((a, co) <- lst) {
      assert(N == a.length)
      assert(N == co.length)
      for ((y, idx) <- a.zipWithIndex) {
        tester.poke(s"io_a_$idx", y)
      }
      tester.step()
      for ((y, idx) <- co.zipWithIndex) {
        tester.expect(s"io_co_$idx", y)
      }
      tester.step()
    }

    tester.report()

  }
}
