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
