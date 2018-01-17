;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-10-02 23:42:46.711, builtAtMillis: 1506987766711
circuit WithResetTest : 
  module WithResetTest : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire reset2 : UInt<1>
    reset2 is invalid
    reset2 <= UInt<1>("h00")
    node _T_4 = bits(reset, 0, 0) @[MultiClockSpec.scala 58:39]
    node _T_5 = or(reset2, _T_4) @[MultiClockSpec.scala 58:30]
    reg reg : UInt<8>, clock with : (reset => (_T_5, UInt<8>("h00"))) @[MultiClockSpec.scala 58:56]
    node _T_9 = add(reg, UInt<1>("h01")) @[MultiClockSpec.scala 59:14]
    node _T_10 = tail(_T_9, 1) @[MultiClockSpec.scala 59:14]
    reg <= _T_10 @[MultiClockSpec.scala 59:7]
    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_15 = eq(value, UInt<4>("h09")) @[Counter.scala 25:24]
      node _T_17 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_18 = tail(_T_17, 1) @[Counter.scala 26:22]
      value <= _T_18 @[Counter.scala 26:13]
      when _T_15 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_15) @[Counter.scala 63:20]
    node _T_21 = lt(value, UInt<3>("h07")) @[MultiClockSpec.scala 62:15]
    when _T_21 : @[MultiClockSpec.scala 62:22]
      node _T_22 = eq(reg, value) @[MultiClockSpec.scala 63:16]
      node _T_23 = bits(reset, 0, 0) @[MultiClockSpec.scala 63:11]
      node _T_24 = or(_T_22, _T_23) @[MultiClockSpec.scala 63:11]
      node _T_26 = eq(_T_24, UInt<1>("h00")) @[MultiClockSpec.scala 63:11]
      when _T_26 : @[MultiClockSpec.scala 63:11]
        printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:63 assert(reg === cycle)\n") @[MultiClockSpec.scala 63:11]
        stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 63:11]
        skip @[MultiClockSpec.scala 63:11]
      skip @[MultiClockSpec.scala 62:22]
    else : @[MultiClockSpec.scala 64:31]
      node _T_28 = eq(value, UInt<3>("h07")) @[MultiClockSpec.scala 64:22]
      when _T_28 : @[MultiClockSpec.scala 64:31]
        reset2 <= UInt<1>("h01") @[MultiClockSpec.scala 65:12]
        skip @[MultiClockSpec.scala 64:31]
      else : @[MultiClockSpec.scala 66:31]
        node _T_31 = eq(value, UInt<4>("h08")) @[MultiClockSpec.scala 66:22]
        when _T_31 : @[MultiClockSpec.scala 66:31]
          node _T_33 = eq(reg, UInt<1>("h00")) @[MultiClockSpec.scala 67:16]
          node _T_34 = bits(reset, 0, 0) @[MultiClockSpec.scala 67:11]
          node _T_35 = or(_T_33, _T_34) @[MultiClockSpec.scala 67:11]
          node _T_37 = eq(_T_35, UInt<1>("h00")) @[MultiClockSpec.scala 67:11]
          when _T_37 : @[MultiClockSpec.scala 67:11]
            printf(clock, UInt<1>(1), "Assertion failed\n    at MultiClockSpec.scala:67 assert(reg === 0.U)\n") @[MultiClockSpec.scala 67:11]
            stop(clock, UInt<1>(1), 1) @[MultiClockSpec.scala 67:11]
            skip @[MultiClockSpec.scala 67:11]
          skip @[MultiClockSpec.scala 66:31]
    when done : @[MultiClockSpec.scala 69:15]
      node _T_38 = bits(reset, 0, 0) @[MultiClockSpec.scala 69:21]
      node _T_40 = eq(_T_38, UInt<1>("h00")) @[MultiClockSpec.scala 69:21]
      when _T_40 : @[MultiClockSpec.scala 69:21]
        stop(clock, UInt<1>(1), 0) @[MultiClockSpec.scala 69:21]
        skip @[MultiClockSpec.scala 69:21]
      skip @[MultiClockSpec.scala 69:15]
    
