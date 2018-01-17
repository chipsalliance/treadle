;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit ParameterizedNumberOperation : 
  module ParameterizedNumberOperation : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a1 : Fixed<16><<8>>, flip a2 : Fixed<16><<8>>, c : Fixed<48><<8>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg register1 : Fixed<48><<8>>, clock @[ParameterizedOpSpec.scala 27:22]
    node _T_10 = mul(io.a1, io.a2) @[FixedPointTypeClass.scala 43:59]
    register1 <= _T_10 @[ParameterizedOpSpec.scala 29:13]
    io.c <= register1 @[ParameterizedOpSpec.scala 39:8]
    
