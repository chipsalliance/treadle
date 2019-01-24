// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class MultiClockMemorySpec extends FreeSpec with Matchers {
  "MultiClockMemorySpec should pass a basic test" ignore {
    val input =
      """
        |circuit MultiClockMemTest :
        |  module MultiClockMemTest :
        |    input clock : Clock
        |    input reset : UInt<1>
        |
        |    reg cDiv : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), cDiv) @[MultiClockSpec.scala 74:21]
        |    mem mem : @[MultiClockSpec.scala 78:16]
        |      data-type => UInt<32>
        |      depth => 8
        |      read-latency => 0
        |      write-latency => 1
        |      reader => rdata
        |      writer => _T_27
        |      writer => _T_48
        |      read-under-write => undefined
        |    reg value : UInt<5>, clock with :
        |      reset => (UInt<1>("h0"), value) @[Counter.scala 26:33]
        |    reg waddr : UInt<3>, clock with :
        |      reset => (UInt<1>("h0"), waddr) @[MultiClockSpec.scala 83:22]
        |    node _T_8 = eq(cDiv, UInt<1>("h0")) @[MultiClockSpec.scala 75:11]
        |    node clock2 = asClock(cDiv) @[MultiClockSpec.scala 76:21]
        |    node _T_15 = eq(value, UInt<5>("h13")) @[Counter.scala 34:24]
        |    node _T_17 = add(value, UInt<1>("h1")) @[Counter.scala 35:22]
        |    node _T_18 = tail(_T_17, 1) @[Counter.scala 35:22]
        |    node _GEN_0 = mux(_T_15, UInt<1>("h0"), _T_18) @[Counter.scala 37:21]
        |    node _GEN_1 = mux(UInt<1>("h1"), _GEN_0, value) @[Counter.scala 63:17]
        |    node done = and(UInt<1>("h1"), _T_15) @[Counter.scala 64:20]
        |    node _T_23 = add(waddr, UInt<1>("h1")) @[MultiClockSpec.scala 84:18]
        |    node _T_24 = tail(_T_23, 1) @[MultiClockSpec.scala 84:18]
        |    node _T_26 = lt(value, UInt<4>("h8")) @[MultiClockSpec.scala 85:15]
        |    node _GEN_2 = validif(_T_26, waddr) @[MultiClockSpec.scala 85:22]
        |    node _GEN_3 = validif(_T_26, clock) @[MultiClockSpec.scala 85:22]
        |    node _GEN_4 = mux(_T_26, UInt<1>("h1"), UInt<1>("h0")) @[MultiClockSpec.scala 85:22]
        |    node _GEN_5 = validif(_T_26, UInt<7>("h7b")) @[MultiClockSpec.scala 85:22]
        |    node _T_30 = sub(waddr, UInt<1>("h1")) @[MultiClockSpec.scala 89:21]
        |    node _T_31 = asUInt(_T_30) @[MultiClockSpec.scala 89:21]
        |    node raddr = tail(_T_31, 1) @[MultiClockSpec.scala 89:21]
        |    node _T_33 = gt(value, UInt<1>("h0")) @[MultiClockSpec.scala 93:15]
        |    node _T_35 = lt(value, UInt<4>("h9")) @[MultiClockSpec.scala 93:30]
        |    node _T_36 = and(_T_33, _T_35) @[MultiClockSpec.scala 93:21]
        |    node _T_38 = eq(mem.rdata.data, UInt<7>("h7b")) @[MultiClockSpec.scala 94:18]
        |    node _T_39 = bits(reset, 0, 0) @[MultiClockSpec.scala 94:11]
        |    node _T_40 = or(_T_38, _T_39) @[MultiClockSpec.scala 94:11]
        |    node _T_42 = eq(_T_40, UInt<1>("h0")) @[MultiClockSpec.scala 94:11]
        |    node _T_44 = geq(value, UInt<4>("h8")) @[MultiClockSpec.scala 99:17]
        |    node _T_46 = lt(value, UInt<5>("h10")) @[MultiClockSpec.scala 99:33]
        |    node _T_47 = and(_T_44, _T_46) @[MultiClockSpec.scala 99:24]
        |    node _GEN_6 = validif(_T_47, waddr) @[MultiClockSpec.scala 99:41]
        |    node _GEN_7 = validif(_T_47, clock2) @[MultiClockSpec.scala 99:41]
        |    node _GEN_8 = mux(_T_47, UInt<1>("h1"), UInt<1>("h0")) @[MultiClockSpec.scala 99:41]
        |    node _GEN_9 = validif(_T_47, UInt<9>("h1c8")) @[MultiClockSpec.scala 99:41]
        |    node _T_51 = gt(value, UInt<4>("h8")) @[MultiClockSpec.scala 105:15]
        |    node _T_53 = lt(value, UInt<5>("h11")) @[MultiClockSpec.scala 105:30]
        |    node _T_54 = and(_T_51, _T_53) @[MultiClockSpec.scala 105:21]
        |    node _T_56 = rem(raddr, UInt<2>("h2")) @[MultiClockSpec.scala 106:17]
        |    node _T_58 = eq(_T_56, UInt<1>("h0")) @[MultiClockSpec.scala 106:23]
        |    node _T_60 = eq(mem.rdata.data, UInt<9>("h1c8")) @[MultiClockSpec.scala 107:20]
        |    node _T_61 = bits(reset, 0, 0) @[MultiClockSpec.scala 107:13]
        |    node _T_62 = or(_T_60, _T_61) @[MultiClockSpec.scala 107:13]
        |    node _T_64 = eq(_T_62, UInt<1>("h0")) @[MultiClockSpec.scala 107:13]
        |    node _T_66 = eq(mem.rdata.data, UInt<7>("h7b")) @[MultiClockSpec.scala 109:20]
        |    node _T_67 = bits(reset, 0, 0) @[MultiClockSpec.scala 109:13]
        |    node _T_68 = or(_T_66, _T_67) @[MultiClockSpec.scala 109:13]
        |    node _T_70 = eq(_T_68, UInt<1>("h0")) @[MultiClockSpec.scala 109:13]
        |    node _T_71 = bits(reset, 0, 0) @[MultiClockSpec.scala 113:21]
        |    node _T_73 = eq(_T_71, UInt<1>("h0")) @[MultiClockSpec.scala 113:21]
        |    cDiv <= mux(reset, UInt<1>("h1"), _T_8)
        |    mem.rdata.addr <= raddr
        |    mem.rdata.en <= UInt<1>("h1")
        |    mem.rdata.clk <= clock
        |    mem._T_27.addr <= _GEN_2
        |    mem._T_27.en <= _GEN_4
        |    mem._T_27.clk <= _GEN_3
        |    mem._T_27.data <= _GEN_5
        |    mem._T_27.mask <= _GEN_4
        |    mem._T_48.addr <= _GEN_6
        |    mem._T_48.en <= _GEN_8
        |    mem._T_48.clk <= _GEN_7
        |    mem._T_48.data <= _GEN_9
        |    mem._T_48.mask <= _GEN_8
        |    value <= mux(reset, UInt<5>("h0"), _GEN_1)
        |    waddr <= mux(reset, UInt<3>("h0"), _T_24)
        |    printf(clock, and(and(and(UInt<1>("h1"), _T_36), _T_42), UInt<1>("h1")), "Assertion failed\n    at MultiClockSpec.scala:94 assert(rdata === 123.U)\n") @[MultiClockSpec.scala 94:11]
        |    stop(clock, and(and(and(UInt<1>("h1"), _T_36), _T_42), UInt<1>("h1")), 1) @[MultiClockSpec.scala 94:11]
        |    printf(clock, and(and(and(and(UInt<1>("h1"), _T_54), _T_58), _T_64), UInt<1>("h1")), "Assertion failed\n    at MultiClockSpec.scala:107 assert(rdata === 456.U)\n") @[MultiClockSpec.scala 107:13]
        |    stop(clock, and(and(and(and(UInt<1>("h1"), _T_54), _T_58), _T_64), UInt<1>("h1")), 1) @[MultiClockSpec.scala 107:13]
        |    printf(clock, and(and(and(and(UInt<1>("h1"), _T_54), eq(_T_58, UInt<1>("h0"))), _T_70), UInt<1>("h1")), "Assertion failed\n    at MultiClockSpec.scala:109 assert(rdata === 123.U)\n") @[MultiClockSpec.scala 109:13]
        |    stop(clock, and(and(and(and(UInt<1>("h1"), _T_54), eq(_T_58, UInt<1>("h0"))), _T_70), UInt<1>("h1")), 1) @[MultiClockSpec.scala 109:13]
        |    stop(clock, and(and(and(UInt<1>("h1"), done), _T_73), UInt<1>("h1")), 0) @[MultiClockSpec.scala 113:21]
        |
      """.stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD           = false,
        vcdShowUnderscored = false,
        setVerbose         = false,
        showFirrtlAtLoad   = false,
        rollbackBuffers    = 4,
        callResetAtStartUp = true,
        symbolsToWatch     = Seq()
      )
    }

    val tester = TreadleTester(input, optionsManager)

    tester.step(100)
    tester.report()
  }
}
