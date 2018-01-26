// See LICENSE for license details.
package treadle

import org.scalatest.{FlatSpec, Matchers}

class PrintStopSpec extends FlatSpec with Matchers {
  behavior of "stop"

  it should "return not stop if condition is not met" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(0), 2) ; Can't happen!
        |
      """.stripMargin

    val engine = ExecutionEngine(input)

    for (cycle_number <- 0 to 10) {
      engine.doCycles(2)
      engine.stopped should be (false)
    }
  }

  it should "return failure if a stop with non-zero result" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(1), 2) ; Failure!
        |
      """.stripMargin

    val engine = ExecutionEngine(input)

    intercept[StopException] {
      engine.doCycles(2)
    }
    engine.stopped should be (true)
    engine.lastStopResult.get should be (2)
  }

  it should "return success if a stop with zero result" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(1), 0) ; Success!
        |
      """.stripMargin

    val engine = ExecutionEngine(input)

    intercept[StopException] {
      engine.doCycles(2)
    }
    engine.stopped should be (true)
    engine.lastStopResult.get should be (0)
  }

  behavior of "Print statement"

  it should "be visible" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "HELLO WORLD\n")
        |
      """.stripMargin

    val engine = ExecutionEngine(input)

    engine.doCycles(2)

  }
  it should "support printf formatting" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "HELLO WORLD int %d hex %x SIint %d\n", UInt(7), UInt(31), SInt(-2) )
        |
      """.stripMargin

    val engine = ExecutionEngine(input)

    engine.doCycles(2)

  }


  it should "support printf formatting with binary" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "char %c int %d hex %x SIint %d %b\n", UInt(77), UInt(7), UInt(255), SInt(-2), SInt(7) )
        |    printf(clk, UInt(1), "char %c int %d hex %x SIint %d %b\n", UInt(48), UInt(7), UInt(255), SInt(-2), SInt(-7) )
        |
        """.stripMargin

    val engine = ExecutionEngine(input)

    engine.doCycles(2)

  }

  it should "print at the right part of clock cycle" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clock : Clock
        |    input reset: UInt<1>
        |    input in1 : UInt<16>
        |    input in2 : UInt<16>
        |    input enable: UInt<1>
        |
        |    node x = add(in1, UInt<1>("h1"))
        |
        |    reg reg : UInt<8>, clock with :
        |      reset => (reset, UInt<8>("h0"))
        |
        |    reg <= add(reg, UInt<1>("h1"))
        |    node clockInt = asUInt(clock)
        |
        |    printf(clock, enable, "in1: %d, x : %d, reg: %d, clock %d, enable: %d\n", in1, x, reg, clockInt, enable)
        |
        """.stripMargin

    val tester = new TreadleTester(input)
    tester.poke("enable", 0)
    tester.poke("in1", 1)
    println("before peek")
    println(s"x ${tester.peek("x")}")
    println("after peek")

    tester.poke("in2", 2)
    println("before cycle")
    tester.step()
    println("after cycle")
    println("before peek")
    println(s"x ${tester.peek("x")}")
    println("after peek")

    tester.poke("enable", 1)
    tester.poke("in1", 1)
    println("before peek")
    println(s"x ${tester.peek("x")}")
    println("after peek")

    tester.poke("in2", 2)
    println("before cycle")
    tester.step()
    println("after cycle")
    println("before peek")
    println(s"x ${tester.peek("x")}")
    println("after peek")

    println("before peek")
    tester.step()
    println("after peek")
  }
}
