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

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PrintfTimingSpec extends AnyFreeSpec with Matchers {
  "printf has strict timing requirements" - {
    "it must fire before registers are updated" in {
      val input =
        """
          |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
          |circuit Printf1 :
          |  module Printf1 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |
          |    reg reg0 : UInt<8>, clock
          |    reg0 <= add(reg0, UInt(1))
          |
          |    node wire0 = add(reg0, UInt(1))
          |
          |    printf(clock, UInt<1>(1), "reg0=%x wire0=%x\n", reg0, wire0)
          |""".stripMargin

      val treadleTester = TreadleTester(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation))
      treadleTester.step(10)
      treadleTester.finish
    }
    "printf every other time based on reg" in {
      val input =
        """
          |circuit Printf2 :
          |  module Printf2 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |
          |    reg reg0 : UInt<8>, clock
          |    reg0 <= add(reg0, UInt(1))
          |
          |    node enable = eq(add(reg0, UInt(4)), UInt(0))
          |
          |    printf(clock, enable, "reg0=%x\n", reg0)
          |
          |""".stripMargin

      val treadleTester = TreadleTester(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation))
      treadleTester.step(10)
      treadleTester.finish
    }
  }
}
