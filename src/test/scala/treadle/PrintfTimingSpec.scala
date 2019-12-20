// See README.md for license details.

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

private class PrintfTimingSpec extends FreeSpec with Matchers {
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
          |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.7
          |circuit Printf2 :
          |  module Printf2 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |
          |    reg reg0 : UInt<8>, clock
          |    reg0 <= add(reg0, UInt(1))
          |
          |    node enable = eq(mod(reg0, UInt(4)), UInt(0))
          |
          |    printf(clock, enable, "reg0=%x\n", reg0)
          |""".stripMargin

      val treadleTester = TreadleTester(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation))
      treadleTester.step(10)
      treadleTester.finish
    }
  }
}
