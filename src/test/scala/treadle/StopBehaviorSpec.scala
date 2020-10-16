// SPDX-License-Identifier: Apache-2.0

package treadle

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.executable.StopException

class StopBehaviorSpec extends AnyFreeSpec with Matchers {
  val input: String =
    """
      |circuit myRisc :
      |  module mockRegFileOut :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_addr : UInt<8>
      |    output io_DataO : UInt<32>
      |
      |    node T_3 = eq(io_addr, UInt<1>("h0")) @[RunTimeAssertSpec.scala 16:16]
      |    node GEN_0 = validif(T_3, UInt<8>("h80")) @[RunTimeAssertSpec.scala 16:28]
      |    node T_6 = eq(T_3, UInt<1>("h0")) @[RunTimeAssertSpec.scala 16:28]
      |    node T_8 = eq(reset, UInt<1>("h0")) @[RunTimeAssertSpec.scala 19:11]
      |    node T_10 = eq(reset, UInt<1>("h0")) @[RunTimeAssertSpec.scala 20:22]
      |    io_DataO <= GEN_0
      |    printf(clk, and(and(and(UInt<1>("h1"), T_6), T_8), UInt<1>("h1")), "STOP:Read at the wrong Register\n") @[RunTimeAssertSpec.scala 19:11]
      |    stop(clk, and(and(and(UInt<1>("h1"), T_6), T_10), UInt<1>("h1")), 47) @[RunTimeAssertSpec.scala 20:22]
      |
      |  module myRisc :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_wrData : UInt<32>
      |    output io_valid : UInt<1>
      |    output io_out : UInt<32>
      |
      |    inst mockRegFileOut_1 of mockRegFileOut @[RunTimeAssertSpec.scala 36:20]
      |    node rci = bits(io_wrData, 23, 16) @[RunTimeAssertSpec.scala 42:18]
      |    node T_4 = eq(rci, UInt<8>("hff")) @[RunTimeAssertSpec.scala 44:13]
      |    node GEN_0 = validif(T_4, UInt<1>("h1")) @[RunTimeAssertSpec.scala 44:27]
      |    node GEN_1 = validif(T_4, mockRegFileOut_1.io_DataO) @[RunTimeAssertSpec.scala 44:27]
      |    node T_7 = eq(T_4, UInt<1>("h0")) @[RunTimeAssertSpec.scala 44:27]
      |    node GEN_2 = mux(T_7, UInt<1>("h0"), GEN_0) @[RunTimeAssertSpec.scala 47:14]
      |    io_valid <= GEN_2
      |    io_out <= GEN_1
      |    mockRegFileOut_1.io_addr <= UInt<5>("h10")
      |    mockRegFileOut_1.clk <= clk
      |    mockRegFileOut_1.reset <= reset
      |
    """.stripMargin

  "Stop should abort engine immediately" in {

    Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>

        tester.poke("reset", 0)
        tester.poke("io_wrData", (0 << 24) + (255 << 16))
        tester.expect("reset", 0)

        intercept[StopException] {
          tester.step()
        }

        tester.reportString should include("Failed: Stop result 47")
      }
    }
  }

  "stop should say stopped if return value is 0" in {
    val input: String =
      """
        |circuit HasStop0 :
        |  module HasStop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output count : UInt<32>
        |
        |    reg counter : UInt<32>, clock with : (reset => (reset, UInt(0)))
        |
        |    counter <= tail(add(counter, UInt("h1")), 1)
        |
        |    count <= counter
        |
        |    stop(clock, eq(counter, UInt("h20")), 0)
        |
    """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val caught = intercept[StopException] {
        tester.step(100)
        tester.finish
      }
      caught.getMessage should include("Stopped: result 0")

      tester.getStopResult should be(Some(0))
    }
  }

  "stop should say failed if return value is > 0" in {
    val input: String =
      """
        |circuit HasStop0 :
        |  module HasStop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output count : UInt<32>
        |
        |    reg counter : UInt<32>, clock with : (reset => (reset, UInt(0)))
        |
        |    counter <= tail(add(counter, UInt("h1")), 1)
        |
        |    count <= counter
        |
        |    stop(clock, eq(counter, UInt("h20")), 44)
        |
    """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val caught = intercept[StopException] {
        tester.step(100)
        tester.finish
      }
      caught.getMessage should include("Failure Stop: result 44")

      tester.getStopResult should be(Some(44))
    }
  }
}
