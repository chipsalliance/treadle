// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class ClockedManuallySpec extends FreeSpec with Matchers {

  private val input =
    """
      |circuit ManuallyClocked :
      |  module ManuallyClocked :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input in1 : UInt<8>
      |    output out_direct : UInt<8>
      |    output out_from_reg : UInt<8>
      |    output inverted_clock : Clock
      |    output out_from_neg_edge_reg : UInt<8>
      |
      |    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h03")))
      |    reg reg2 : UInt<8>, inverted_clock with : (reset => (reset, UInt<8>("h07")))
      |
      |    out_direct <= in1
      |
      |    reg1 <= in1
      |    out_from_reg <= reg1
      |
      |    reg2 <= in1
      |    out_from_neg_edge_reg <= reg2
      |
      |    inverted_clock <= asClock(not(asUInt(clock)))
      |
      """.stripMargin

  "Circuit can be run, just with poking and time advance" in {

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        vcdShowUnderscored = true,
        writeVCD = true
      )
      commonOptions = commonOptions.copy(
        targetDirName = "test_run_dir/manually_clocked_pos",
        topName = "manually_clocked_pos"
      )
    }

    val tester = new TreadleTester(input, optionsManager)

    tester.advanceTime(300)

    tester.poke("clock", 0)
    tester.poke("reset", 0)
    tester.poke("in1", 0)

    tester.expect("out_direct", 0)
    tester.expect("out_from_reg", 0)
    tester.expect("inverted_clock", 1)

    tester.advanceTime(100)

    tester.poke("in1", 3)

    tester.expect("out_direct", 3)
    tester.expect("out_from_reg", 0)
    tester.expect("inverted_clock", 1)

    tester.poke("clock", 1)

    tester.expect("out_direct", 3)
    tester.expect("out_from_reg", 3)
    tester.expect("inverted_clock", 0)

    tester.advanceTime(100)

    tester.expect("out_direct", 3)
    tester.expect("out_from_reg", 3)
    tester.expect("inverted_clock", 0)

    tester.report()
  }

  "clock up should advance registers off negative edge" in {

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = true,
        vcdShowUnderscored = true,
        writeVCD = true
      )
      commonOptions = commonOptions.copy(
        targetDirName = "test_run_dir/manually_clocked_neg",
        topName = "neg_reg"
      )
    }

    val tester = new TreadleTester(input, optionsManager)

    tester.advanceTime(10)

    tester.poke("in1", 1)
    tester.poke("clock", 1)
    tester.poke("reset", 0)

    tester.expect("out_direct", 1)
    tester.expect("out_from_reg", 0)
    tester.expect("inverted_clock", 0)

    tester.advanceTime(10)

    tester.poke("in1", 2)
    tester.poke("clock", 0)

    tester.expect("out_direct", 2)
    tester.expect("out_from_reg", 0)
    tester.expect("inverted_clock", 1)

    tester.advanceTime(10)

    tester.poke("clock", 1)

    tester.expect("out_direct", 2)
    tester.expect("out_from_reg", 2)
    tester.expect("inverted_clock", 0)

    tester.report()
  }

}
