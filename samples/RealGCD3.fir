;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit RealGCD3 : 
  module RealGCD3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[AdvTesterSpec.scala 40:14]
    reg y : UInt<16>, clock @[AdvTesterSpec.scala 41:14]
    reg p : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[AdvTesterSpec.scala 42:18]
    reg ti : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[AdvTesterSpec.scala 44:19]
    node _T_20 = add(ti, UInt<1>("h01")) @[AdvTesterSpec.scala 45:12]
    node _T_21 = tail(_T_20, 1) @[AdvTesterSpec.scala 45:12]
    ti <= _T_21 @[AdvTesterSpec.scala 45:6]
    node _T_23 = eq(p, UInt<1>("h00")) @[AdvTesterSpec.scala 47:18]
    io.in.ready <= _T_23 @[AdvTesterSpec.scala 47:15]
    node _T_25 = eq(p, UInt<1>("h00")) @[AdvTesterSpec.scala 49:24]
    node _T_26 = and(io.in.valid, _T_25) @[AdvTesterSpec.scala 49:21]
    when _T_26 : @[AdvTesterSpec.scala 49:28]
      x <= io.in.bits.a @[AdvTesterSpec.scala 50:7]
      y <= io.in.bits.b @[AdvTesterSpec.scala 51:7]
      p <= UInt<1>("h01") @[AdvTesterSpec.scala 52:7]
      skip @[AdvTesterSpec.scala 49:28]
    when p : @[AdvTesterSpec.scala 55:12]
      node _T_28 = gt(x, y) @[AdvTesterSpec.scala 56:13]
      when _T_28 : @[AdvTesterSpec.scala 56:19]
        x <= y @[AdvTesterSpec.scala 56:23]
        y <= x @[AdvTesterSpec.scala 56:31]
        skip @[AdvTesterSpec.scala 56:19]
      node _T_30 = eq(_T_28, UInt<1>("h00")) @[AdvTesterSpec.scala 56:19]
      when _T_30 : @[AdvTesterSpec.scala 57:21]
        node _T_31 = sub(y, x) @[AdvTesterSpec.scala 57:30]
        node _T_32 = asUInt(_T_31) @[AdvTesterSpec.scala 57:30]
        node _T_33 = tail(_T_32, 1) @[AdvTesterSpec.scala 57:30]
        y <= _T_33 @[AdvTesterSpec.scala 57:25]
        skip @[AdvTesterSpec.scala 57:21]
      node _T_35 = eq(reset, UInt<1>("h00")) @[AdvTesterSpec.scala 58:11]
      when _T_35 : @[AdvTesterSpec.scala 58:11]
        printf(clock, UInt<1>(1), "x: %d, y: %d\n", x, y) @[AdvTesterSpec.scala 58:11]
        skip @[AdvTesterSpec.scala 58:11]
      skip @[AdvTesterSpec.scala 55:12]
    node _T_37 = eq(p, UInt<1>("h00")) @[AdvTesterSpec.scala 55:12]
    when _T_37 : @[AdvTesterSpec.scala 59:15]
      node _T_39 = eq(reset, UInt<1>("h00")) @[AdvTesterSpec.scala 59:23]
      when _T_39 : @[AdvTesterSpec.scala 59:23]
        printf(clock, UInt<1>(1), "stalled\n") @[AdvTesterSpec.scala 59:23]
        skip @[AdvTesterSpec.scala 59:23]
      skip @[AdvTesterSpec.scala 59:15]
    io.out.bits <= x @[AdvTesterSpec.scala 61:16]
    node _T_41 = eq(y, UInt<1>("h00")) @[AdvTesterSpec.scala 62:21]
    node _T_42 = and(_T_41, p) @[AdvTesterSpec.scala 62:29]
    io.out.valid <= _T_42 @[AdvTesterSpec.scala 62:16]
    when io.out.valid : @[AdvTesterSpec.scala 63:23]
      p <= UInt<1>("h00") @[AdvTesterSpec.scala 64:7]
      skip @[AdvTesterSpec.scala 63:23]
    
