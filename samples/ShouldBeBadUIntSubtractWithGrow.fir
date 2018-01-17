;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit ShouldBeBadUIntSubtractWithGrow : 
  module ShouldBeBadUIntSubtractWithGrow : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<4>, flip b : UInt<4>, o : UInt<4>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = sub(io.a, io.b) @[NumbersSpec.scala 215:60]
    node _T_6 = asUInt(_T_5) @[NumbersSpec.scala 215:60]
    node _T_7 = tail(_T_6, 1) @[NumbersSpec.scala 215:60]
    reg r : UInt, clock @[NumbersSpec.scala 215:18]
    r <= _T_7 @[NumbersSpec.scala 215:18]
    io.o <= r @[NumbersSpec.scala 216:8]
    
