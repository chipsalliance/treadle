;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit ClockDividerTest : 
  module ClockDividerTest : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg cDiv : UInt<1>, clock with : (reset => (reset, UInt<1>("h01"))) @[MultiClockSpec.scala 12:21]
    node _T_5 = eq(cDiv, UInt<1>("h00")) @[MultiClockSpec.scala 13:11]
    cDiv <= _T_5 @[MultiClockSpec.scala 13:8]
    node clock2 = asClock(cDiv) @[MultiClockSpec.scala 14:21]
    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[MultiClockSpec.scala 16:21]
    node _T_9 = add(reg1, UInt<1>("h01")) @[MultiClockSpec.scala 17:16]
    node _T_10 = tail(_T_9, 1) @[MultiClockSpec.scala 17:16]
    reg1 <= _T_10 @[MultiClockSpec.scala 17:8]
    reg reg2 : UInt<8>, clock2 with : (reset => (reset, UInt<8>("h00"))) @[MultiClockSpec.scala 18:41]
    node _T_14 = add(reg2, UInt<1>("h01")) @[MultiClockSpec.scala 19:16]
    node _T_15 = tail(_T_14, 1) @[MultiClockSpec.scala 19:16]
    reg2 <= _T_15 @[MultiClockSpec.scala 19:8]
    node _T_17 = lt(reg1, UInt<4>("h0a")) @[MultiClockSpec.scala 21:14]
    when _T_17 : @[MultiClockSpec.scala 21:22]
      node _T_19 = div(reg1, UInt<2>("h02")) @[MultiClockSpec.scala 22:26]
      node _T_20 = eq(reg2, _T_19) @[MultiClockSpec.scala 22:17]
      node _T_21 = bits(reset, 0, 0) @[MultiClockSpec.scala 22:11]
      node _T_22 = or(_T_20, _T_21) @[MultiClockSpec.scala 22:11]
      node _T_24 = eq(_T_22, UInt<1>("h00")) @[MultiClockSpec.scala 22:11]
      when _T_24 : @[MultiClockSpec.scala 22:11]
        printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:22 assert(reg2 === reg1 / 2.U) // 1:2 clock relationship\n") @[MultiClockSpec.scala 22:11]
        stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 22:11]
        skip @[MultiClockSpec.scala 22:11]
      skip @[MultiClockSpec.scala 21:22]
    node _T_26 = eq(reg1, UInt<4>("h0a")) @[MultiClockSpec.scala 25:14]
    when _T_26 : @[MultiClockSpec.scala 25:24]
      node _T_27 = bits(reset, 0, 0) @[MultiClockSpec.scala 26:9]
      node _T_29 = eq(_T_27, UInt<1>("h00")) @[MultiClockSpec.scala 26:9]
      when _T_29 : @[MultiClockSpec.scala 26:9]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 26:9]
        skip @[MultiClockSpec.scala 26:9]
      skip @[MultiClockSpec.scala 25:24]
    
