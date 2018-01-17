;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-25 20:31:59.290, builtAtMillis: 1506371519290
circuit FixedPointDivide : 
  module FixedPointDivide : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<10><<4>>, out : Fixed<10><<4>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_2 = asUInt(io.in) @[FixedPointSpec.scala 39:20]
    node _T_3 = shr(_T_2, 2) @[FixedPointSpec.scala 39:27]
    node _T_4 = asFixedPoint(_T_3, 4) @[FixedPointSpec.scala 39:55]
    io.out <= _T_4 @[FixedPointSpec.scala 39:10]
    
