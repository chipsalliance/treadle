;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-25 20:31:59.290, builtAtMillis: 1506371519290
circuit HasCycle : 
  module HasCycle : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, o : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire b : UInt<1> @[CycleTest.scala 16:15]
    b is invalid @[CycleTest.scala 16:15]
    node _T_5 = and(b, io.a) @[CycleTest.scala 17:9]
    b <= _T_5 @[CycleTest.scala 17:5]
    io.o <= b @[CycleTest.scala 19:8]
    
