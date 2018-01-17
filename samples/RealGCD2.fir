;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-09-06 18:39:16.536, builtAtMillis: 1504723156536
circuit RealGCD2 : 
  module RealGCD2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[GCDSpec.scala 38:14]
    reg y : UInt<16>, clock @[GCDSpec.scala 39:14]
    reg p : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[GCDSpec.scala 40:18]
    reg ti : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[GCDSpec.scala 42:19]
    node _T_20 = add(ti, UInt<1>("h01")) @[GCDSpec.scala 43:12]
    node _T_21 = tail(_T_20, 1) @[GCDSpec.scala 43:12]
    ti <= _T_21 @[GCDSpec.scala 43:6]
    node _T_23 = eq(p, UInt<1>("h00")) @[GCDSpec.scala 45:18]
    io.in.ready <= _T_23 @[GCDSpec.scala 45:15]
    node _T_25 = eq(p, UInt<1>("h00")) @[GCDSpec.scala 47:24]
    node _T_26 = and(io.in.valid, _T_25) @[GCDSpec.scala 47:21]
    when _T_26 : @[GCDSpec.scala 47:28]
      x <= io.in.bits.a @[GCDSpec.scala 48:7]
      y <= io.in.bits.b @[GCDSpec.scala 49:7]
      p <= UInt<1>("h01") @[GCDSpec.scala 50:7]
      skip @[GCDSpec.scala 47:28]
    when p : @[GCDSpec.scala 53:12]
      node _T_28 = gt(x, y) @[GCDSpec.scala 54:13]
      when _T_28 : @[GCDSpec.scala 54:19]
        x <= y @[GCDSpec.scala 54:23]
        y <= x @[GCDSpec.scala 54:31]
        skip @[GCDSpec.scala 54:19]
      else : @[GCDSpec.scala 55:21]
        node _T_29 = sub(y, x) @[GCDSpec.scala 55:30]
        node _T_30 = asUInt(_T_29) @[GCDSpec.scala 55:30]
        node _T_31 = tail(_T_30, 1) @[GCDSpec.scala 55:30]
        y <= _T_31 @[GCDSpec.scala 55:25]
        skip @[GCDSpec.scala 55:21]
      skip @[GCDSpec.scala 53:12]
    io.out.bits <= x @[GCDSpec.scala 58:16]
    node _T_33 = eq(y, UInt<1>("h00")) @[GCDSpec.scala 59:21]
    node _T_34 = and(_T_33, p) @[GCDSpec.scala 59:29]
    io.out.valid <= _T_34 @[GCDSpec.scala 59:16]
    when io.out.valid : @[GCDSpec.scala 60:23]
      p <= UInt<1>("h00") @[GCDSpec.scala 61:7]
      skip @[GCDSpec.scala 60:23]
    
