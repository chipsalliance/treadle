;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-08-25 22:51:12.887, builtAtMillis: 1503701472887
circuit BBWrapper : 
  extmodule BBAnd : 
    output result : UInt<1>
    input b : UInt<1>
    input a : UInt<1>
    
    defname = BBAnd
    
    
  module BBWrapper : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, result : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst tb of BBAnd @[BlackBoxAnd.scala 27:18]
    tb.result is invalid
    tb.b is invalid
    tb.a is invalid
    tb.a <= io.a @[BlackBoxAnd.scala 28:11]
    tb.b <= io.b @[BlackBoxAnd.scala 29:11]
    io.result <= tb.result @[BlackBoxAnd.scala 30:13]
    
