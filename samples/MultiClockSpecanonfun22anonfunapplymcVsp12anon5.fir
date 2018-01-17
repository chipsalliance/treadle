;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit MultiClockSpecanonfun22anonfunapplymcVsp12anon5 : 
  module MultiClockSpecanonfun22anonfunapplymcVsp12anon5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = eq(UInt<1>("h00"), UInt<1>("h01")) @[MultiClockSpec.scala 164:28]
    node _T_7 = or(_T_5, UInt<1>("h01")) @[MultiClockSpec.scala 164:23]
    node _T_9 = eq(_T_7, UInt<1>("h00")) @[MultiClockSpec.scala 164:23]
    when _T_9 : @[MultiClockSpec.scala 164:23]
      printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:164 chisel3.assert(0.U === 1.U)\n") @[MultiClockSpec.scala 164:23]
      stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 164:23]
      skip @[MultiClockSpec.scala 164:23]
    reg value : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_14 = eq(value, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_16 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_17 = tail(_T_16, 1) @[Counter.scala 26:22]
      value <= _T_17 @[Counter.scala 26:13]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_14) @[Counter.scala 63:20]
    when done : @[MultiClockSpec.scala 167:19]
      node _T_18 = bits(reset, 0, 0) @[MultiClockSpec.scala 167:25]
      node _T_20 = eq(_T_18, UInt<1>("h00")) @[MultiClockSpec.scala 167:25]
      when _T_20 : @[MultiClockSpec.scala 167:25]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 167:25]
        skip @[MultiClockSpec.scala 167:25]
      skip @[MultiClockSpec.scala 167:19]
    
