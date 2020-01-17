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

// scalastyle:off magic.number
class ExpressionRenderSpec extends FreeSpec with Matchers {
  "ExpressionRenderSpec should pass a basic test" in {
    val input =
      """
        |circuit DynamicMemorySearch :
        |  module DynamicMemorySearch :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    input in1 : UInt<16>
        |    input in2 : UInt<16>
        |    input in3 : UInt<16>
        |    input sel0 : UInt<1>
        |    input sel1 : UInt<1>
        |    input sel2 : UInt<1>
        |    input sel3 : UInt<1>
        |    output out : UInt<16>
        |
        |    node node3 = mux(sel3, in3, UInt<16>("hf"))
        |    node node2 = mux(sel2, in2, node3)
        |    node node1 = mux(sel1, in1, node2)
        |    node node0 = mux(sel0, in0, node1)
        |
        |    out <= node0
      """.stripMargin

    val t = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
    t.poke("in0", 10)
    t.poke("in1", 11)
    t.poke("in2", 12)
    t.poke("in3", 13)

    t.poke("sel1", 1)

    println(s"out = ${t.peek("out")}")
    println(t.engine.renderComputation("out"))
  }

  "ExpressionRenderSpec show register values from previous time" in {
    val input =
      """
        |circuit DynamicMemorySearch :
        |  module DynamicMemorySearch :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    output out0 : UInt<16>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |    output out3 : UInt<16>
        |
        |    reg r1 : UInt<16>, clock
        |    reg r2 : UInt<16>, clock
        |    reg r3 : UInt<16>, clock
        |
        |    r1 <= in0
        |    r2 <= r1
        |    r3 <= r2
        |
        |    out0 <= in0
        |    out1 <= r1
        |    out2 <= r2
        |    out3 <= r3
        |
      """.stripMargin

    val t = TreadleTester(Seq(FirrtlSourceAnnotation(input), RollBackBuffersAnnotation(10)))
    t.poke("in0", 1)
    t.step()
    t.poke("in0", 2)
    t.step()
    t.poke("in0", 3)
    t.step()
    t.poke("in0", 4)
    t.step()
    t.poke("in0", 5)
    t.step()

    println(t.engine.renderComputation("out0"))
    println(t.engine.renderComputation("r1"))
    println(t.engine.renderComputation("r2"))
    println(t.engine.renderComputation("r3"))
  }

}
