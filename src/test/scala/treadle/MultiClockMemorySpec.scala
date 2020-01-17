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

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.{ClockInfo, TreadleException}

// scalastyle:off magic.number
class MultiClockMemorySpec extends FreeSpec with Matchers {
  "should work with two-clocks with different periods" in {
    val input =
      """
        |circuit MultiClockMemTest :
        |  module MultiClockMemTest :
        |    input clock1 : Clock
        |    input clock2 : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock1 with : (reset => (reset, UInt<8>("h0")))
        |    reg reg2 : UInt<16>, clock2 with : (reset => (reset, UInt<8>("h0")))
        |
        |    reg1 <= add(reg1, UInt<16>("h1"))
        |    reg2 <= add(reg2, UInt<16>("h1"))
        |
        |    out1 <= reg1
        |    out2 <= reg2
      """.stripMargin

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
      TargetDirAnnotation("test_run_dir/two-clock-test"),
      WriteVcdAnnotation,
      RollBackBuffersAnnotation(4),
      CallResetAtStartupAnnotation,
      ClockInfoAnnotation(Seq(ClockInfo("clock1", 6), ClockInfo("clock2", 2)))
    )

    val tester = TreadleTester(annotations)

    var r1 = 0
    var r2 = 1

    tester.poke("reset", 1)
    tester.step()
    tester.poke("reset", 0)

    for (trial <- 1 to 6) {
      tester.step()
      println(f"trial $trial%3d -- ${tester.peek("out1")}%6d ${tester.peek("out2")}%6d")

      tester.peek("out1") should be(r1)
      tester.peek("out2") should be(r2)

      if (trial % 3 == 1) r1 += 1
      r2 += 1
    }
    tester.report()
  }

  "clock period must be divisible by two" in {
    val input =
      """
        |circuit MultiClockMemTest :
        |  module MultiClockMemTest :
        |    input clock1 : Clock
        |    input clock2 : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock1 with : (reset => (reset, UInt<8>("h0")))
        |    reg reg2 : UInt<16>, clock2 with : (reset => (reset, UInt<8>("h0")))
        |
        |    reg1 <= add(reg1, UInt<16>("h1"))
        |    reg2 <= add(reg2, UInt<16>("h1"))
        |
        |    out1 <= reg1
        |    out2 <= reg2
      """.stripMargin

    val thrown = intercept[TreadleException] {
      TreadleTester(
        Seq(FirrtlSourceAnnotation(input), ClockInfoAnnotation(Seq(ClockInfo("clock1", 3), ClockInfo("clock2", 1))))
      )
    }
    thrown.message should be("Error: Clock period must be divisible by 2: Found ClockInfo(clock1,3,1)")
  }
}
