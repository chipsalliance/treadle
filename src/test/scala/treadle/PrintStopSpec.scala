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

    val interpreter = ExecutionEngine(input)

    for (cycle_number <- 0 to 10) {
      interpreter.doCycles(2)
      interpreter.stopped should be (false)
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

    val interpreter = ExecutionEngine(input)

    intercept[StopException] {
      interpreter.doCycles(2)
    }
    interpreter.stopped should be (true)
    interpreter.lastStopResult.get should be (2)
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

    val interpreter = ExecutionEngine(input)

    intercept[StopException] {
      interpreter.doCycles(2)
    }
    interpreter.stopped should be (true)
    interpreter.lastStopResult.get should be (0)
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

    val interpreter = ExecutionEngine(input)

    interpreter.doCycles(2)

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

    val interpreter = ExecutionEngine(input)

    interpreter.doCycles(2)

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

    val interpreter = ExecutionEngine(input)

    interpreter.doCycles(2)

  }
}
