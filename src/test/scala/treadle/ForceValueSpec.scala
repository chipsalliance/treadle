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

//scalastyle:off magic.number
class ForceValueSpec extends FreeSpec with Matchers {
  val simpleCircuit: String =
    s"""
       |circuit SimpleCircuit :
       |  module SimpleCircuit :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    input in_a : UInt<16>
       |    input in_b : UInt<16>
       |    input in_c : UInt<70>
       |
       |    output out_a : UInt<16>
       |    output out_a_plus_b : UInt<16>
       |    output out_c : UInt<70>
       |    output wire_out : UInt<16>
       |    output reg_out : UInt<16>
       |
       |    reg x : UInt<16>, clock with :
       |      reset => (UInt<1>("h0"), x)
       |
       |    x <= in_a
       |    reg_out <= x
       |
       |    out_a <= in_a
       |    out_a_plus_b <= add(in_a, in_b)
       |    out_c <= in_c
       |
       |    node wire_1 = add(in_b, in_b)
       |    node wire_2 = add(wire_1, in_a)
       |
       |    wire_out <= wire_2
       |
       """.stripMargin

  "force value operates on any internal wire" - {
    "no forces should work as expected" in {

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(simpleCircuit)))

      val bigNum = BigInt("1" * 66, 2)

      tester.poke("in_a", 7)
      tester.poke("in_b", 77)
      tester.poke("in_c", bigNum)

      tester.step()

      tester.expect("out_a", 7)
      tester.expect("wire_out", 161)
      tester.expect("reg_out", 7)
      tester.expect("out_a_plus_b", 84)
      tester.expect("out_c", bigNum)
    }
    "force register should work" in {

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(simpleCircuit)))

      val bigNum = BigInt("1" * 66, 2)

      tester.poke("in_a", 7)
      tester.poke("in_b", 77)
      tester.poke("in_c", bigNum)

      tester.forceValue("x", 3)

      tester.step()

      tester.expect("out_a", 7)
      tester.expect("wire_out", 161)
      tester.expect("reg_out", 3)
      tester.expect("out_a_plus_b", 84)
      tester.expect("out_c", bigNum)
    }
    "force wire should work" in {

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(simpleCircuit)))

      val bigNum = BigInt("1" * 66, 2)

      tester.poke("in_a", 7)
      tester.poke("in_b", 77)
      tester.poke("in_c", bigNum)

      tester.step()

      tester.forceValue("wire_1", 10)

      tester.expect("out_a", 7)
      tester.expect("wire_out", 17)
      tester.expect("reg_out", 7)
      tester.expect("out_a_plus_b", 84)
      tester.expect("out_c", bigNum)

      tester.clearForceValue("wire_1")

      tester.expect("wire_out", 161)

    }
  }
}
