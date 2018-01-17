;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit TrimTypeCircuit : 
  module TrimTypeCircuit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : Fixed<4><<2>>, flip b : Fixed<4><<2>>, multiplyRoundHalfUp : Fixed<10><<5>>, multiplyNoTrim : Fixed<20><<7>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_2 = mul(io.a, io.b) @[FixedPointTypeClass.scala 43:59]
    reg regMultiplyRoundHalfUp : Fixed<<4>>, clock @[NumbersSpec.scala 154:39]
    regMultiplyRoundHalfUp <= _T_2 @[NumbersSpec.scala 154:39]
    node _T_4 = mul(io.a, io.b) @[FixedPointTypeClass.scala 43:59]
    reg regMultiplyNoTrim : Fixed<<4>>, clock @[NumbersSpec.scala 157:34]
    regMultiplyNoTrim <= _T_4 @[NumbersSpec.scala 157:34]
    io.multiplyRoundHalfUp <= regMultiplyRoundHalfUp @[NumbersSpec.scala 161:26]
    io.multiplyNoTrim <= regMultiplyNoTrim @[NumbersSpec.scala 162:21]
    
