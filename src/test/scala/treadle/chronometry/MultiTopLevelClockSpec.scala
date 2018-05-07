// See LICENSE for license details.

package treadle.chronometry

import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.ClockInfo
import treadle.{TreadleOptionsManager, TreadleTester}


// scalastyle:off magic.number
class MultiTopLevelClockSpec extends FreeSpec with Matchers {
  val input : String =
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
    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        clockInfo = Seq(ClockInfo("clock1", period = 17, 1000), ClockInfo("clock2", period = 19, initialOffset = 1017)),
        showFirrtlAtLoad = false,
        callResetAtStartUp = true,
        writeVCD = false
      )
    }
    val tester = TreadleTester(input, optionsManager)

    for(_ <- 0 until 17 * 19 + 10) {
//    for(_ <- 0 until 30) {
      println(s"state = ${tester.peek("out1")}, ${tester.peek("out2")}")
      tester.step()
    }

    tester.finish
  }
}
