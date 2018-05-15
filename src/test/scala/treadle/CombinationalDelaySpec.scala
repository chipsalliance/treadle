// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.ClockInfo

//scalastyle:off magic.number
class CombinationalDelaySpec extends FreeSpec with Matchers {
  private val input =
    """
      |circuit CombinationalCircuit :
      |
      |  module CombinationalCircuit :
      |    input clock    : Clock
      |    input reset    : SInt<16>
      |    input in_0     : SInt<16>
      |    input in_1     : SInt<16>
      |    output add_out : SInt<16>
      |    output sub_out : SInt<16>
      |    output eq_out  : UInt<1>
      |
      |    add_out <= add(in_0, in_1)
      |    sub_out <= sub(in_0, in_1)
      |    eq_out  <= eq(in_0, in_1)
      |
    """.stripMargin

  "combinational delay takes 100th of period to execute" in {
    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = false,
        clockInfo = Seq(ClockInfo("clock", period = 1000L, initialOffset = 0)),
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        rollbackBuffers = 0,
        symbolsToWatch = Seq()
      )
    }

    val t = new TreadleTester(input, optionsManager)
    t.poke("in_0", 20)
    t.poke("in_1", 11)
    t.expect("add_out", 31)
    t.expect("sub_out", 9)
    t.expect("eq_out", 0)

    assert(t.wallTime.currentTime == 10)

    t.poke("in_0", 5)
    t.poke("in_1", 25)
    t.expect("add_out", 30)
    t.expect("sub_out", -20)
    t.expect("eq_out", 0)

    assert(t.wallTime.currentTime == 20)

    t.poke("in_0", 5)
    t.poke("in_1", 5)
    t.expect("add_out", 10)
    t.expect("sub_out", 0)
    t.expect("eq_out", 1)

    assert(t.wallTime.currentTime == 30)

    t.step()

    assert(t.wallTime.currentTime == 1000)

    t.report()

  }
}
