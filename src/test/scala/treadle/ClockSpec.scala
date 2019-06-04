// See LICENSE for license details.

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.StopException


// scalastyle:off magic.number
class ClockSpec extends FreeSpec with Matchers {
  "ClockSpec should pass a basic test" in {
    val input =
      """
        |circuit Stop0 :
        |  module DUT :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out2 : UInt<8>
        |
        |    reg reg2 : UInt<8>, clock with : (reset => (reset, UInt<8>("h07")))
        |    reg2 <= add(reg2, UInt<8>("h01"))
        |    out2 <= reg2
        |
        |  module Stop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<8>
        |    output out2 : UInt<8>
        |
        |    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h03")))
        |
        |    inst dut of DUT
        |
        |    dut.clock <= clock
        |    dut.reset <= reset
        |
        |    out1  <= reg1
        |    out2  <= dut.out2
        |
        |    reg1 <= add(reg1, UInt<8>("h01"))
        |    when gt(reg1, UInt<8>("h08")) :
        |      stop(clock, UInt(1), 0) ; Done!
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    intercept[StopException] {
      tester.step(100)
    }
    tester.engine.lastStopResult should be (Some(0))
    tester.report()
  }

  "clocks must behave properly behind validif" in {
    val input =
      """
        |circuit ClockedValidIf :
        |  module ClockedValidIf :
        |    input clock    : Clock
        |    input reset    : UInt<1>
        |    input in1      : UInt<16>
        |    input valid1   : UInt<1>
        |    input valid2   : UInt<1>
        |    input addr     : UInt<8>
        |    input write_en : UInt<1>
        |    output out1    : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt<8>("h07")))
        |    reg1 <= add(in1, UInt<16>(1))
        |
        |    node clock2 = validif(valid1, clock)
        |
        |    mem m :
        |      data-type => UInt<16>
        |      depth => 8
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |
        |    m.read.clk <= clock2
        |    m.read.en <= eq(write_en, UInt<1>(0))
        |    m.read.addr <= addr
        |
        |    m.write.clk <= clock2
        |    m.write.en <= eq(write_en, UInt<1>(1))
        |    m.write.mask <= UInt<8>("hff")
        |    m.write.addr <= addr
        |    m.write.data <= in1
        |
        |    out1 <= m.read.data
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        vcdShowUnderscored = true,
        showFirrtlAtLoad = false,
        writeVCD = false,
        validIfIsRandom = false
      )
    }

    val tester = TreadleTester(input, optionsManager)

    // load memory
    tester.poke("write_en", 1)
    for(i <- 0 until 8) {
      tester.poke("addr", i)
      tester.poke("in1", i * 10 + i)
      tester.step()
    }

    for(i <- 0 until 8) {
      println(s"memory($i) = ${tester.peekMemory("m", i)}")
    }
    // read phase
    tester.poke("write_en", 1)
    for(i <- 0 until 8) {
      tester.poke("addr", i)
      tester.expect("out1", i * 10 + i)

      println(s"mem($i) ${tester.peekMemory("m", i)}")
      tester.step()
    }

    tester.report()
  }
}
