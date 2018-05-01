// See LICENSE for license details.

package treadle

import firrtl.ExecutionOptionsManager
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

    val t = TreadleTester(Array("--fint-write-vcd",
                                "--fint-vcd-show-underscored-vars",
                                "--fint-verbose",
                                "--show-firrtl-at-load",
                                "--fint-rollback-buffers", "0",
                                "--firrtl-source", input))
    t.poke("in0", 10)
    t.poke("in1", 11)
    t.poke("in2", 12)
    t.poke("in3", 13)

    t.poke("sel1", 1)

    println(s"out = ${t.peek("out")}")
    println(t.engine.renderComputation("out"))
  }

}
