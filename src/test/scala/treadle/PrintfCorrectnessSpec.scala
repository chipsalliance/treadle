// See README.md for license details.

package treadle

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

class PrintfCorrectnessSpec extends FreeSpec with Matchers {
  "printf needs to capture values at the proper time" in {
    val input =
      """
        |circuit HasPrintf :
        |  module HasPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input moveHead : UInt<1>
        |    input moveTail : UInt<1>
        |    output tailIsHead : UInt<1>
        |    output nextTailIsHead : UInt<1>
        |
        |    reg headPointer : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), headPointer) @[PrintfTreadleVsVerilatorTest.scala 33:28]
        |    reg tailPointer : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), tailPointer) @[PrintfTreadleVsVerilatorTest.scala 34:28]
        |    reg count : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), count) @[PrintfTreadleVsVerilatorTest.scala 35:22]
        |    node _T = add(count, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 36:18]
        |    node _T_1 = tail(_T, 1) @[PrintfTreadleVsVerilatorTest.scala 36:18]
        |    node _T_2 = add(tailPointer, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_3 = tail(_T_2, 1) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_5 = geq(_T_3, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 25:22]
        |    node _T_6 = sub(_T_3, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _T_7 = tail(_T_6, 1) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _GEN_0 = mux(_T_5, _T_7, _T_3) @[PrintfTreadleVsVerilatorTest.scala 25:38]
        |    skip
        |    node nextTail = tail(_GEN_0, 1) @[PrintfTreadleVsVerilatorTest.scala 30:16]
        |    node _T_8 = add(headPointer, UInt<8>("h1")) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_9 = tail(_T_8, 1) @[PrintfTreadleVsVerilatorTest.scala 23:31]
        |    node _T_11 = geq(_T_9, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 25:22]
        |    node _T_12 = sub(_T_9, UInt<8>("h4")) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _T_13 = tail(_T_12, 1) @[PrintfTreadleVsVerilatorTest.scala 26:29]
        |    node _GEN_1 = mux(_T_11, _T_13, _T_9) @[PrintfTreadleVsVerilatorTest.scala 25:38]
        |    skip
        |    node _T_14 = tail(_GEN_1, 1) @[PrintfTreadleVsVerilatorTest.scala 30:16]
        |    node _GEN_2 = mux(moveHead, pad(_T_14, 8), headPointer) @[PrintfTreadleVsVerilatorTest.scala 40:18]
        |    node _GEN_4 = mux(moveTail, pad(nextTail, 8), tailPointer) @[PrintfTreadleVsVerilatorTest.scala 43:18]
        |    node _GEN_5 = pad(nextTail, 8) @[PrintfTreadleVsVerilatorTest.scala 48:30]
        |    skip
        |    node _T_25 = eq(reset, UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    tailIsHead <= eq(tailPointer, headPointer) @[PrintfTreadleVsVerilatorTest.scala 47:14]
        |    skip
        |    nextTailIsHead <= eq(_GEN_5, headPointer) @[PrintfTreadleVsVerilatorTest.scala 48:18]
        |    headPointer <= mux(reset, UInt<8>("h0"), _GEN_2) @[PrintfTreadleVsVerilatorTest.scala 41:17]
        |    tailPointer <= mux(reset, UInt<8>("h0"), _GEN_4) @[PrintfTreadleVsVerilatorTest.scala 44:17]
        |    count <= mux(reset, UInt<8>("h0"), _T_1) @[PrintfTreadleVsVerilatorTest.scala 36:9]
        |    reg _GEN_3 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_3 <= count @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    reg _GEN_6 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_6 <= headPointer @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    reg _GEN_7 : UInt<8>, clock with :
        |      reset => (UInt<1>("h0"), UInt<1>("h0")) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    _GEN_7 <= tailPointer @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |    printf(clock, _T_25, "PRINTF:%d moveHead %d, moveTail %d, head %d, tail %d, nextTail %d\n", _GEN_3, moveHead, moveTail, _GEN_6, _GEN_7, nextTail) @[PrintfTreadleVsVerilatorTest.scala 50:9]
        |
        |""".stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
      tester.step()
      tester.poke("moveTail", 1)
      tester.step()
      tester.step()
      tester.finish
    }
    Seq(
      "PRINTF:0 moveHead 0, moveTail 0, head 0, tail 0, nextTail 1",
      "PRINTF:1 moveHead 0, moveTail 1, head 0, tail 0, nextTail 2",
      "PRINTF:2 moveHead 0, moveTail 1, head 0, tail 1, nextTail 3"
    ).foreach { targetLine =>
      output.toString.contains(targetLine) should be (true)
    }
  }
}
