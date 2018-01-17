;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit MultiClockSubModuleTest : 
  module MultiClockSubModuleTestSubModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {out : UInt}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_7 = eq(value, UInt<4>("h09")) @[Counter.scala 25:24]
      node _T_9 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_10 = tail(_T_9, 1) @[Counter.scala 26:22]
      value <= _T_10 @[Counter.scala 26:13]
      when _T_7 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      skip @[Counter.scala 62:17]
    node _T_12 = and(UInt<1>("h01"), _T_7) @[Counter.scala 63:20]
    io.out <= value @[MultiClockSpec.scala 36:12]
    
  module MultiClockSubModuleTest : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_6 = eq(value, UInt<4>("h09")) @[Counter.scala 25:24]
      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_9 = tail(_T_8, 1) @[Counter.scala 26:22]
      value <= _T_9 @[Counter.scala 26:13]
      when _T_6 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 63:20]
    reg cDiv : UInt<1>, clock with : (reset => (reset, UInt<1>("h01"))) @[MultiClockSpec.scala 40:21]
    node _T_14 = eq(cDiv, UInt<1>("h00")) @[MultiClockSpec.scala 41:11]
    cDiv <= _T_14 @[MultiClockSpec.scala 41:8]
    node otherClock = asClock(cDiv) @[MultiClockSpec.scala 43:25]
    node otherReset = lt(value, UInt<2>("h03")) @[MultiClockSpec.scala 44:26]
    inst inst of MultiClockSubModuleTestSubModule @[MultiClockSpec.scala 46:64]
    inst.io is invalid
    inst.clock <= otherClock
    inst.reset <= otherReset
    when done : @[MultiClockSpec.scala 48:15]
      node _T_17 = eq(inst.io.out, UInt<2>("h03")) @[MultiClockSpec.scala 50:24]
      node _T_18 = bits(reset, 0, 0) @[MultiClockSpec.scala 50:11]
      node _T_19 = or(_T_17, _T_18) @[MultiClockSpec.scala 50:11]
      node _T_21 = eq(_T_19, UInt<1>("h00")) @[MultiClockSpec.scala 50:11]
      when _T_21 : @[MultiClockSpec.scala 50:11]
        printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:50 assert(inst.io.out === 3.U)\n") @[MultiClockSpec.scala 50:11]
        stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 50:11]
        skip @[MultiClockSpec.scala 50:11]
      node _T_22 = bits(reset, 0, 0) @[MultiClockSpec.scala 51:9]
      node _T_24 = eq(_T_22, UInt<1>("h00")) @[MultiClockSpec.scala 51:9]
      when _T_24 : @[MultiClockSpec.scala 51:9]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 51:9]
        skip @[MultiClockSpec.scala 51:9]
      skip @[MultiClockSpec.scala 48:15]
    
