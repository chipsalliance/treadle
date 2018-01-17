;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit MultiClockSpecanonfun22anonfunapplymcVsp13anon6 : 
  module MultiClockSpecanonfun22anonfunapplymcVsp13anon6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_3 = asClock(UInt<1>("h00")) @[MultiClockSpec.scala 171:33]
    node _T_6 = eq(UInt<1>("h00"), UInt<1>("h01")) @[MultiClockSpec.scala 172:28]
    node _T_7 = bits(reset, 0, 0) @[MultiClockSpec.scala 172:23]
    node _T_8 = or(_T_6, _T_7) @[MultiClockSpec.scala 172:23]
    node _T_10 = eq(_T_8, UInt<1>("h00")) @[MultiClockSpec.scala 172:23]
    when _T_10 : @[MultiClockSpec.scala 172:23]
      printf(_T_3, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:172 chisel3.assert(0.U === 1.U)\n") @[MultiClockSpec.scala 172:23]
      stop(_T_3, UInt<1>(1), 1) @[MultiClockSpec.scala 172:23]
      skip @[MultiClockSpec.scala 172:23]
    reg value : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_15 = eq(value, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_17 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_18 = tail(_T_17, 1) @[Counter.scala 26:22]
      value <= _T_18 @[Counter.scala 26:13]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_15) @[Counter.scala 63:20]
    when done : @[MultiClockSpec.scala 175:19]
      node _T_19 = bits(reset, 0, 0) @[MultiClockSpec.scala 175:25]
      node _T_21 = eq(_T_19, UInt<1>("h00")) @[MultiClockSpec.scala 175:25]
      when _T_21 : @[MultiClockSpec.scala 175:25]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 175:25]
        skip @[MultiClockSpec.scala 175:25]
      skip @[MultiClockSpec.scala 175:19]
    
