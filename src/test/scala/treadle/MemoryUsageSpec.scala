// See LICENSE for license details.
package treadle

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by chick on 4/30/16.
  */
class MemoryUsageSpec extends FlatSpec with Matchers {

  behavior of "chirrtl mems"

  it should "parse and run ok" in {
    val chirrtlMemInput =
      """
        |circuit ChirrtlMems :
        |  module ChirrtlMems :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    mem ram :
        |      data-type => UInt<32>
        |      depth => 16
        |      read-latency => 0
        |      write-latency => 1
        |      reader => r
        |      writer => w
        |      read-under-write => undefined
        |    node newClock = clock
        |    wire wen : UInt<1>
        |    reg raddr : UInt<4>, clock with :
        |      reset => (reset, UInt<1>("h0"))
        |    node newerClock = clock
        |    reg waddr : UInt<4>, clock with :
        |      reset => (reset, UInt<1>("h0"))
        |    node GEN_0 = not(reset)
        |    node GEN_1 = gt(waddr, UInt<1>("h1"))
        |    node GEN_2 = and(GEN_0, GEN_1)
        |    node GEN_3 = neq(ram.r.data, raddr)
        |    node GEN_4 = and(GEN_2, GEN_3)
        |    printf(clock, GEN_4, "Assertion failed! r =/= raddr\n")
        |    node GEN_5 = not(reset)
        |    node GEN_6 = gt(waddr, UInt<1>("h1"))
        |    node GEN_7 = and(GEN_5, GEN_6)
        |    node GEN_8 = neq(ram.r.data, raddr)
        |    node GEN_9 = and(GEN_7, GEN_8)
        |    stop(clock, GEN_9, 1)
        |    node GEN_10 = not(reset)
        |    node GEN_11 = eq(raddr, UInt<4>("hf"))
        |    node GEN_12 = and(GEN_10, GEN_11)
        |    stop(clock, GEN_12, 0)
        |    ram.r.addr <= raddr
        |    ram.r.en <= UInt<1>("h1")
        |    ram.r.clk <= clock
        |    ram.w.data <= validif(wen, waddr)
        |    ram.w.mask <= wen
        |    ram.w.addr <= validif(wen, waddr)
        |    ram.w.en <= wen
        |    ram.w.clk <= validif(wen, clock)
        |    wen <= not(reset)
        |    node GEN_13 = eq(waddr, UInt<1>("h0"))
        |    node GEN_14 = add(raddr, UInt<1>("h1"))
        |    node GEN_15 = mux(GEN_13, UInt<1>("h0"), GEN_14)
        |    node GEN_16 = add(raddr, UInt<1>("h1"))
        |    node GEN_17 = mux(wen, GEN_15, GEN_16)
        |    raddr <= bits(GEN_17, 3, 0)
        |    node GEN_18 = add(waddr, UInt<1>("h1"))
        |    waddr <= bits(GEN_18, 3, 0)
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(showFirrtlAtLoad = false, setVerbose = false)
    }
    val tester = new TreadleTester(chirrtlMemInput, optionsManager) {
      poke("reset", 1)
      step()
      poke("reset", 0)
      step()
    }
    tester.report()
  }

  behavior of "memory primitives"

  it should "run this circuit" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clock : Clock
        |    input a : UInt<1>
        |    input b : UInt<1>
        |    input select : UInt<1>
        |    output c : UInt<1>
        |    mem m :
        |      data-type => { a : UInt<8>, b : UInt<8>}[2]
        |      depth => 32
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |    m.read.clk <= clock
        |    m.read.en <= UInt<1>(1)
        |    m.read.addr is invalid
        |    node x = m.read.data
        |    node y = m.read.data[0].b
        |
        |    m.write.clk <= clock
        |    m.write.en <= UInt<1>(0)
        |    m.write.mask is invalid
        |    m.write.addr is invalid
        |    wire w : { a : UInt<8>, b : UInt<8>}[2]
        |    w[0].a <= UInt<4>(2)
        |    w[0].b <= UInt<4>(3)
        |    w[1].a <= UInt<4>(4)
        |    w[1].b <= UInt<4>(5)
        |    m.write.data <= w
        |    c <= a
      """.stripMargin

    val tester = new TreadleTester(input) {
      poke("a", 1)
      poke("b", 0)
      poke("select", 0)

      step()

      def testC(): Unit = {
        val m = peek("c")
        println(s"got $m")
        step()
      }
      testC()
    }
    tester.report()
  }

  behavior of "read-write memory"

  it should "work with a simple example" in {
    val depth = 2
    val input =
      s"""
        |circuit target_memory :
        |  module target_memory :
        |    input clock      : Clock
        |    input index      : UInt<12>
        |    input do_write   : UInt<1>
        |    input do_enable  : UInt<1>
        |    input write_data : UInt<12>
        |    output read_data : UInt<12>
        |
        |    mem ram :
        |      data-type => UInt<12>
        |      depth => $depth
        |      read-latency => 1
        |      write-latency => 1
        |      readwriter => RW_0
        |      read-under-write => undefined
        |
        |    ram.RW_0.clk <= clock
        |    ram.RW_0.addr <= index
        |    ram.RW_0.en <= UInt<1>("h1")
        |
        |    ram.RW_0.wmode <= do_write
        |    read_data <= ram.RW_0.rdata
        |    ram.RW_0.wdata <= write_data
        |    ram.RW_0.wmask <= UInt<1>("h1")
      """.stripMargin

    val tester = new TreadleTester(input) {
      // setVerbose(true)

      poke("do_write", 1)
      for(i <- 0 until depth) {
        poke("index", i)
        poke("write_data", i + 3)
        step()
      }
      poke("do_write", 0)
      step(2)

      for(i <- 0 until depth) {
        poke("index", i)
        step()
        expect("read_data", i + 3)
      }
    }
    tester.report()
  }

  it should "run this more complex circuit" in {
    val input =
      """
        |circuit target_memory :
        |  module target_memory :
        |    input clock : Clock
        |    input outer_addr : UInt<11>
        |    input outer_din : UInt<12>
        |    output outer_dout : UInt<12>
        |    input outer_write_en : UInt<1>
        |
        |    node outer_addr_sel = bits(outer_addr, 10, 10)
        |    reg outer_addr_sel_reg : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), outer_addr_sel_reg)
        |    outer_addr_sel_reg <= mux(UInt<1>("h1"), outer_addr_sel, outer_addr_sel_reg)
        |    inst mem_0_0 of awesome_lib_mem
        |    mem_0_0.lib_clk <= clock
        |    mem_0_0.lib_addr <= outer_addr
        |    node outer_dout_0_0 = bits(mem_0_0.lib_dout, 11, 0)
        |    mem_0_0.lib_din <= bits(outer_din, 11, 0)
        |    mem_0_0.lib_write_en <= and(and(outer_write_en, UInt<1>("h1")), eq(outer_addr_sel, UInt<1>("h0")))
        |    node outer_dout_0 = outer_dout_0_0
        |    inst mem_1_0 of awesome_lib_mem
        |    mem_1_0.lib_clk <= clock
        |    mem_1_0.lib_addr <= outer_addr
        |    node outer_dout_1_0 = bits(mem_1_0.lib_dout, 11, 0)
        |    mem_1_0.lib_din <= bits(outer_din, 11, 0)
        |    mem_1_0.lib_write_en <= and(and(outer_write_en, UInt<1>("h1")), eq(outer_addr_sel, UInt<1>("h1")))
        |    node outer_dout_1 = outer_dout_1_0
        |    outer_dout <= mux(eq(outer_addr_sel_reg, UInt<1>("h0")), outer_dout_0, mux(eq(outer_addr_sel_reg, UInt<1>("h1")), outer_dout_1, UInt<1>("h0")))
        |
        |  module awesome_lib_mem :
        |    input lib_clk : Clock
        |    input lib_addr : UInt<10>
        |    input lib_din : UInt<12>
        |    output lib_dout : UInt<12>
        |    input lib_write_en : UInt<1>
        |
        |    mem ram :
        |      data-type => UInt<12>
        |      depth => 16
        |      read-latency => 1
        |      write-latency => 1
        |      readwriter => RW_0
        |      read-under-write => undefined
        |    ram.RW_0.clk <= lib_clk
        |    ram.RW_0.addr <= lib_addr
        |    ram.RW_0.en <= UInt<1>("h1")
        |    ram.RW_0.wmode <= lib_write_en
        |    lib_dout <= ram.RW_0.rdata
        |    ram.RW_0.wdata <= lib_din
        |    ram.RW_0.wmask <= UInt<1>("h1")
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(showFirrtlAtLoad = false, setVerbose = false)
    }
    val tester = new TreadleTester(input, optionsManager) {
      // setVerbose(true)

      poke("outer_write_en", 1)
      for(i <- 0 until 10) {
        poke("outer_addr", i)
        poke("outer_din", i * 3)
        step()
      }
      poke("outer_write_en", 0)
      step(2)

      for(i <- 0 until 10) {
        poke("outer_addr", i)
        step()
        expect("outer_dout", i * 3)
      }
    }
    tester.report()
  }
}
