;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit MultiClockSpecanonfun22anonfunapplymcVsp11anon4 : 
  module MultiClockSpecanonfun22anonfunapplymcVsp11anon4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_4 = eq(UInt<1>("h00"), UInt<1>("h01")) @[MultiClockSpec.scala 156:28]
    node _T_5 = bits(reset, 0, 0) @[MultiClockSpec.scala 156:23]
    node _T_6 = or(_T_4, _T_5) @[MultiClockSpec.scala 156:23]
    node _T_8 = eq(_T_6, UInt<1>("h00")) @[MultiClockSpec.scala 156:23]
    when _T_8 : @[MultiClockSpec.scala 156:23]
      printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:156 chisel3.assert(0.U === 1.U)\n") @[MultiClockSpec.scala 156:23]
      stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 156:23]
      skip @[MultiClockSpec.scala 156:23]
    reg value : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_13 = eq(value, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_15 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_16 = tail(_T_15, 1) @[Counter.scala 26:22]
      value <= _T_16 @[Counter.scala 26:13]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_13) @[Counter.scala 63:20]
    when done : @[MultiClockSpec.scala 159:19]
      node _T_17 = bits(reset, 0, 0) @[MultiClockSpec.scala 159:25]
      node _T_19 = eq(_T_17, UInt<1>("h00")) @[MultiClockSpec.scala 159:25]
      when _T_19 : @[MultiClockSpec.scala 159:25]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 159:25]
        skip @[MultiClockSpec.scala 159:25]
      skip @[MultiClockSpec.scala 159:19]
    
