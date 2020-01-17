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

package treadle.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.ClockInfo
import treadle.{CallResetAtStartupAnnotation, ClockInfoAnnotation, TreadleTester}

// scalastyle:off magic.number
class MultiTopLevelClockSpec extends FreeSpec with Matchers {
  val input: String =
    """
      |circuit GotClocks : @[:@2.0]
      |  module GotClocks : @[:@14.2]
      |    input clock1 : Clock @[:@15.4]
      |    input clock2 : Clock
      |    input reset : UInt<1> @[:@16.4]
      |    output out1 : UInt<32> @[:@17.4]
      |    output out2 : UInt<32> @[:@17.4]
      |
      |    reg reg1 : UInt<32>, clock1 with :
      |      reset => (UInt<1>("h0"), reg1)
      |
      |    reg reg2 : UInt<32>, clock2 with :
      |      reset => (UInt<1>("h0"), reg2)
      |
      |    reg1 <= mux(reset, UInt<1>(0), add(reg1, UInt<1>(1)))
      |    reg2 <= mux(reset, UInt<1>(0), add(reg2, UInt<1>(1)))
      |
      |    out1 <= reg1
      |    out2 <= reg2
    """.stripMargin

  "ClockMadnessSpec should pass a basic test" in {

    val (period1, period2) = (34, 38)
    val options = Seq(
      CallResetAtStartupAnnotation,
      ClockInfoAnnotation(
        Seq(
          ClockInfo("clock1", period = period1, 1000),
          ClockInfo("clock2", period = period2, initialOffset = 1017)
        )
      )
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    for (_ <- 0 until period1 * period2 + 10) {
      println(s"state = ${tester.peek("out1")}, ${tester.peek("out2")}")
      tester.step()
    }

    tester.finish
  }
}
