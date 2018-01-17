;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit FixedPointAdder : 
  module FixedPointAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : Fixed<16><<2>>, flip in1 : Fixed<16><<2>>, out : Fixed<16><<2>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.in0, io.in1) @[Adder.scala 127:20]
    node _T_6 = tail(_T_5, 1) @[Adder.scala 127:20]
    node _T_7 = asFixedPoint(_T_6, 2) @[Adder.scala 127:20]
    io.out <= _T_7 @[Adder.scala 127:10]
    
