// SPDX-License-Identifier: Apache-2.0

package treadle.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.executable.ClockInfo
import treadle.{CallResetAtStartupAnnotation, ClockInfoAnnotation, TreadleTestHarness}

// scalastyle:off magic.number
class MultiTopLevelClockSpec extends AnyFreeSpec with Matchers with LazyLogging {
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

  //TODO: This needs to be worked out, currently things breake at i == 9
  "Got Clocks should pass a basic test" ignore {

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

    TreadleTestHarness(
      FirrtlSourceAnnotation(input) +: options,
      Array("-cll", this.getClass.getCanonicalName + ":debug")
    ) { tester =>
      for (i <- 0 until period1 * period2 + 10) {
        logger.debug(s"$i ${i / 2} state = ${tester.peek("out1")}, ${tester.peek("out2")}")
        tester.expect("out1", i / 2)
        tester.step()
      }
    }
  }
}
