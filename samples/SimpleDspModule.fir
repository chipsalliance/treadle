;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SimpleDspModule : 
  module SimpleDspModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip x : Fixed<16><<12>>, flip y : Fixed<16><<12>>, z : Fixed<16><<12>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.x, io.y) @[FixedPointTypeClass.scala 25:22]
    reg _T_8 : Fixed<17><<12>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_8 <= _T_5 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_10 : Fixed<17><<12>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_10 <= _T_8 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_12 : Fixed<17><<12>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_12 <= _T_10 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    io.z <= _T_12 @[SimpleDspModuleSpec.scala 37:10]
    
