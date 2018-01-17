;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SimpleAdder : 
  module SimpleAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a1 : Fixed<6><<4>>, flip a2 : Fixed<8><<1>>, c : Fixed<12><<5>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg register1 : Fixed, clock @[SimpleAdderSpec.scala 19:22]
    node _T_6 = add(io.a1, io.a2) @[SimpleAdderSpec.scala 21:22]
    node _T_7 = tail(_T_6, 1) @[SimpleAdderSpec.scala 21:22]
    node _T_8 = asFixedPoint(_T_7, 4) @[SimpleAdderSpec.scala 21:22]
    register1 <= _T_8 @[SimpleAdderSpec.scala 21:13]
    io.c <= register1 @[SimpleAdderSpec.scala 23:8]
    
