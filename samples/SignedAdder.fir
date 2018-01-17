;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit SignedAdder : 
  module SignedAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in0 : SInt<16>, flip in1 : SInt<16>, out : SInt<16>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.in0, io.in1) @[Adder.scala 89:20]
    node _T_6 = tail(_T_5, 1) @[Adder.scala 89:20]
    node _T_7 = asSInt(_T_6) @[Adder.scala 89:20]
    io.out <= _T_7 @[Adder.scala 89:10]
    
