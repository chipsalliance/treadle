;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.11, builtAtString: 2016-11-03 16:53:27.283, builtAtMillis: 1478192007283
circuit RealGCD2 : 
  module RealGCD2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, out : {valid : UInt<1>, bits : UInt<16>}}
    
    io is invalid
    reg x : UInt<16>, clock @[GCDSpec.scala 37:14]
    reg y : UInt<16>, clock @[GCDSpec.scala 38:14]
    reg p : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[GCDSpec.scala 39:14]
    reg ti : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[GCDSpec.scala 41:15]
    node _T_38 = add(ti, UInt<1>("h01")) @[GCDSpec.scala 42:12]
    node _T_39 = tail(_T_38, 1) @[GCDSpec.scala 42:12]
    ti <= _T_39 @[GCDSpec.scala 42:6]
    node _T_41 = eq(p, UInt<1>("h00")) @[GCDSpec.scala 44:18]
    io.in.ready <= _T_41 @[GCDSpec.scala 44:15]
    node _T_43 = eq(p, UInt<1>("h00")) @[GCDSpec.scala 46:24]
    node _T_44 = and(io.in.valid, _T_43) @[GCDSpec.scala 46:21]
    when _T_44 : @[GCDSpec.scala 46:28]
      x <= io.in.bits.a @[GCDSpec.scala 47:7]
      y <= io.in.bits.b @[GCDSpec.scala 48:7]
      p <= UInt<1>("h01") @[GCDSpec.scala 49:7]
      skip @[GCDSpec.scala 46:28]
    when p : @[GCDSpec.scala 52:12]
      node _T_46 = gt(x, y) @[GCDSpec.scala 53:13]
      when _T_46 : @[GCDSpec.scala 53:19]
        x <= y @[GCDSpec.scala 53:23]
        y <= x @[GCDSpec.scala 53:31]
        skip @[GCDSpec.scala 53:19]
      node _T_48 = eq(_T_46, UInt<1>("h00")) @[GCDSpec.scala 53:19]
      when _T_48 : @[GCDSpec.scala 54:21]
        node _T_49 = sub(y, x) @[GCDSpec.scala 54:30]
        node _T_50 = tail(_T_49, 1) @[GCDSpec.scala 54:30]
        y <= _T_50 @[GCDSpec.scala 54:25]
        skip @[GCDSpec.scala 54:21]
      skip @[GCDSpec.scala 52:12]
    io.out.bits <= x @[GCDSpec.scala 57:16]
    node _T_52 = eq(y, UInt<1>("h00")) @[GCDSpec.scala 58:21]
    node _T_53 = and(_T_52, p) @[GCDSpec.scala 58:33]
    io.out.valid <= _T_53 @[GCDSpec.scala 58:16]
    when io.out.valid : @[GCDSpec.scala 59:23]
      p <= UInt<1>("h00") @[GCDSpec.scala 60:7]
      skip @[GCDSpec.scala 59:23]
    
