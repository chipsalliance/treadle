// See LICENSE for license details.
package treadle

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FlatSpec, Matchers}
import treadle.executable.StopException

//scalastyle:off magic.number
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

    val tester = TreadleTester(input)

    for (cycle_number <- 0 to 10) {
      tester.step(2)
      tester.engine.stopped should be (false)
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

    val tester = TreadleTester(input)

    intercept[StopException] {
      tester.step(2)
    }
    tester.engine.stopped should be (true)
    tester.engine.lastStopResult.get should be (2)
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

    val tester = TreadleTester(input)

    intercept[StopException] {
      tester.step(2)
    }
    tester.engine.stopped should be (true)
    tester.engine.lastStopResult.get should be (0)
  }

  it should "have stops happen in order they appear in a module" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit ManyPrintfs :
        |  module ManyPrintfs :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |    io.out <= io.in
        |    stop(clock, UInt<1>(1), 0) @[ManyPrintf.scala 19:11]
        |    stop(clock, UInt<1>(1), 1) @[ManyPrintf.scala 19:11]
        |    stop(clock, UInt<1>(1), 2) @[ManyPrintf.scala 19:11]
        |    stop(clock, UInt<1>(1), 3) @[ManyPrintf.scala 19:11]
      """.stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

      intercept[StopException] {
        tester.step(2)
      }
      tester.engine.stopped should be (true)
      tester.engine.lastStopResult.get should be (0)

    }
  }


  behavior of "Print statement"

  it should "be visible" in {
    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val input =
        """
          |circuit Stop0 :
          |  module Stop0 :
          |    input clk : Clock
          |
          |    printf(clk, UInt(1), "HELLO WORLD\n")
          |
      """.stripMargin

      val manager = new TreadleOptionsManager {
        treadleOptions = treadleOptions.copy(
          showFirrtlAtLoad = false,
          setVerbose = false
        )
      }
      val tester = TreadleTester(input, manager)

      tester.step(2)
      tester.finish
    }
    output.toString().contains("HELLO WORLD") should be (true)
    output.toString.split("\n").count(_.contains("HELLO WORLD")) should be (2)

  }

  it should "support printf formatting" in {
    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val input =
        """
          |circuit Stop0 :
          |  module Stop0 :
          |    input clk : Clock
          |
          |    printf(clk, UInt(1), "HELLO WORLD int %d hex %x SIint %d\n", UInt(7), UInt(31), SInt(-2) )
          |
      """.stripMargin

      val tester = TreadleTester(input)

      tester.step(2)
    }

    output.toString().contains("HELLO WORLD int 7 hex 1f SIint -2") should be (true)

  }


  it should "support printf formatting with binary" in {
    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
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

      val tester = TreadleTester(input)

      tester.step(2)
    }
    output.toString().contains("char M int 7 hex ff SIint -2 111") should be (true)
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

    val tester = TreadleTester(input)
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


  it should "print rry=480 in the following example" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit BadPrintf :
        |  module BadPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |    wire y : UInt<10> @[PrintfWrong.scala 12:15]
        |    reg regY : UInt, clock @[PrintfWrong.scala 13:21]
        |    regY <= y @[PrintfWrong.scala 13:21]
        |    reg regRegY : UInt, clock @[PrintfWrong.scala 14:24]
        |    regRegY <= regY @[PrintfWrong.scala 14:24]
        |    y <= io.in @[PrintfWrong.scala 16:5]
        |    node _T = eq(regRegY, UInt<9>("h01e0")) @[PrintfWrong.scala 18:17]
        |    when _T : @[PrintfWrong.scala 18:28]
        |      node _T_1 = eq(regRegY, UInt<1>("h00")) @[PrintfWrong.scala 19:76]
        |      node _T_2 = eq(regRegY, UInt<9>("h01e0")) @[PrintfWrong.scala 19:93]
        |      node _T_3 = bits(reset, 0, 0) @[PrintfWrong.scala 19:11]
        |      node _T_4 = eq(_T_3, UInt<1>("h00")) @[PrintfWrong.scala 19:11]
        |      when _T_4 : @[PrintfWrong.scala 19:11]
        |        printf(clock, UInt<1>(1), "+++ y=%d ry=%d rry=%d isZero=%x is480=%x\n", y, regY, regRegY, _T_1, _T_2) @[PrintfWrong.scala 19:11]
        |        skip @[PrintfWrong.scala 19:11]
        |      skip @[PrintfWrong.scala 18:28]
        |    io.out <= regRegY @[PrintfWrong.scala 22:10]
        |
        |
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        showFirrtlAtLoad = false
      )
    }

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(input, optionsManager)
      tester.poke("io_in", 479)
      tester.step()

      tester.poke("io_in", 480)
      tester.step()

      tester.poke("io_in", 481)
      tester.step()

    }

    output
      .toString
      .split("\n").count { line => line.contains("+++ y=481 ry=480 rry=479 isZero=0 is480=1") } should be (1)

    output
      .toString
      .split("\n").count { line => line.contains("+++ y=") } should be (1)

  }
  it should "print register values that have not been advanced yet" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit BadPrintf :
        |  module BadPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |    wire y : UInt<10> @[PrintfWrong.scala 12:15]
        |    reg regY : UInt, clock @[PrintfWrong.scala 13:21]
        |    regY <= y @[PrintfWrong.scala 13:21]
        |    reg regRegY : UInt, clock @[PrintfWrong.scala 14:24]
        |    regRegY <= regY @[PrintfWrong.scala 14:24]
        |    y <= io.in @[PrintfWrong.scala 16:5]
        |    node _T = eq(regRegY, UInt<1>("h00")) @[PrintfWrong.scala 19:84]
        |    node _T_1 = eq(regRegY, UInt<9>("h01e0")) @[PrintfWrong.scala 19:101]
        |    node _T_2 = bits(reset, 0, 0) @[PrintfWrong.scala 19:11]
        |    node _T_3 = eq(_T_2, UInt<1>("h00")) @[PrintfWrong.scala 19:11]
        |    when _T_3 : @[PrintfWrong.scala 19:11]
        |      printf(clock, UInt<1>(1), "+++ y=%d ry=%d rry=%d isZero=%x is480=%x\n", y, regY, regRegY, _T, _T_1) @[PrintfWrong.scala 19:11]
        |      skip @[PrintfWrong.scala 19:11]
        |    io.out <= regRegY @[PrintfWrong.scala 22:10]
        |
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        showFirrtlAtLoad = false
      )
    }

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(input, optionsManager)
      tester.poke("io_in", 479)
      tester.step()

      tester.poke("io_in", 480)
      tester.step()

      tester.poke("io_in", 481)
      tester.step(5)

    }

    output
      .toString
      .split("\n").count { line => line.contains("+++ y=481 ry=480 rry=479 isZero=0 is480=1") } should be (1)

    output
      .toString
      .split("\n").count { line => line.contains("+++ y=") } should be (7)

  }
  it should "print swapping register values should show alternating" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit BadPrintf :
        |  module BadPrintf :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |
        |    reg r0 : UInt, clock with :
        |      reset => (reset, UInt<8>("h0"))
        |    reg r1 : UInt, clock with :
        |      reset => (reset, UInt<8>("h1"))
        |    r0 <= r1
        |    r1 <= r0
        |    io.out <= r1
        |    printf(clock, UInt<1>(1), "+++ r0=%d r1=%d\n", r0, r1) @[PrintfWrong.scala 19:11]
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        showFirrtlAtLoad = false
      )
    }

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(input, optionsManager)
      tester.poke("reset", 1)
      tester.step()
      tester.poke("reset", 0)

      tester.step(10)

    }

    val printfLines = output.toString.split("\n").filter(_.startsWith("+++"))

    printfLines.head.contains("+++ r0=0 r1=0") should be (true)

    val linesCorrect = printfLines
            .tail
            .zipWithIndex
            .map { case (line, lineNumber) =>
              println(s"$lineNumber  $line")
              if (lineNumber % 2 == 1) {
                line.contains("+++ r0=1 r1=0")
              }
              else {
                line.contains("+++ r0=0 r1=1")
              }
            }
    println(linesCorrect.map(_.toString).mkString("-"))

    linesCorrect.forall(b => b) should be (true)
  }

  it should "have printf's print in order" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit ManyPrintfs :
        |  module ManyPrintfs :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |    io.out <= io.in
        |    printf(clock, UInt<1>(1), "+++ 0  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, UInt<1>(1), "+++ 1  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, UInt<1>(1), "+++ 2  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, UInt<1>(1), "+++ 3  printf\n") @[ManyPrintf.scala 19:11]
      """.stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

      tester.step(3)

    }

    val printfLines = output.toString.split("\n").filter(_.startsWith("+++"))

    for {
      i <- 0 until 3
      j <- 0 until 4
      line = (i * 4) + j
    } {
      printfLines(line).contains(s"+++ $j") should be (true)
    }
  }

  it should "have printf's print in order taking triggers into account" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
        |circuit ManyPrintfs :
        |  module ManyPrintfs :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<10>, out : UInt<10>}
        |
        |    node T1 = io.in
        |    node T2 = eq(T1, UInt<1>(1))
        |    node T3 = eq(T2, UInt<1>(1))
        |    node T4 = eq(T3, UInt<1>(1))
        |
        |    io.out <= io.in
        |    printf(clock, T4, "+++ 0  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, T3, "+++ 1  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, T2, "+++ 2  printf\n") @[ManyPrintf.scala 19:11]
        |    printf(clock, T1, "+++ 3  printf\n") @[ManyPrintf.scala 19:11]
      """.stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

      tester.poke("io_in", 1)
      tester.step()
      tester.poke("io_in", 0)
      tester.step()
      tester.poke("io_in", 1)
      tester.step()
      tester.poke("io_in", 1)
      tester.step()
    }

    val printfLines = output.toString.split("\n").filter(_.startsWith("+++"))

    for {
      i <- 0 until 2
      j <- 0 until 4
      line = (i * 4) + j
    } {
      printfLines(line).contains(s"+++ $j") should be (true)
    }

  }
}
