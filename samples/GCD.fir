;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-07-19 19:29:28.307, builtAtMillis: 1500492568307
circuit GCD : 
  module GCD : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<16>, flip b : UInt<16>, flip e : UInt<1>, z : UInt<16>, v : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt, clock @[GCD.scala 17:15]
    reg y : UInt, clock @[GCD.scala 18:15]
    node _T_9 = gt(x, y) @[GCD.scala 20:11]
    when _T_9 : @[GCD.scala 20:16]
      node _T_10 = sub(x, y) @[GCD.scala 20:25]
      node _T_11 = asUInt(_T_10) @[GCD.scala 20:25]
      node _T_12 = tail(_T_11, 1) @[GCD.scala 20:25]
      x <= _T_12 @[GCD.scala 20:20]
      skip @[GCD.scala 20:16]
    node _T_14 = eq(_T_9, UInt<1>("h00")) @[GCD.scala 20:16]
    when _T_14 : @[GCD.scala 21:16]
      node _T_15 = sub(y, x) @[GCD.scala 21:25]
      node _T_16 = asUInt(_T_15) @[GCD.scala 21:25]
      node _T_17 = tail(_T_16, 1) @[GCD.scala 21:25]
      y <= _T_17 @[GCD.scala 21:20]
      skip @[GCD.scala 21:16]
    when io.e : @[GCD.scala 23:15]
      x <= io.a @[GCD.scala 23:19]
      y <= io.b @[GCD.scala 23:30]
      skip @[GCD.scala 23:15]
    io.z <= x @[GCD.scala 24:8]
    node _T_19 = eq(y, UInt<1>("h00")) @[GCD.scala 25:13]
    io.v <= _T_19 @[GCD.scala 25:8]
    
