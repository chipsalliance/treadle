// SPDX-License-Identifier: Apache-2.0

package treadle

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import treadle.executable.TreadleException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

//scalastyle:off magic.number
class InfoSpec extends AnyFreeSpec with Matchers {

  "Error in test should show source info" in {
    val input =
      """
        |circuit HasInfo :
        |  module HasInfo :
        |    input clk : Clock
        |    input in1 : UInt<16>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |    out1 <= add(in1, in1)   @[HasInfo.scala 18:51]
        |    out2 <= add(out1, in1)  @[HasInfo.scala 19:51]
        |
      """.stripMargin

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
      tester.poke("in1", 7)

      try {
        tester.expect("out1", 24)
      } catch {
        case t: TreadleException => println(t.getMessage)
        case t: Throwable        => throw t
      }
      tester.expect("out2", 21)

      tester.finish
      tester.report()
    }

    val output = outputBuffer.toString

    output.contains("Assigned at:  @[HasInfo.scala 18:51]") should be(true)

    println(output.toString)
  }
}
