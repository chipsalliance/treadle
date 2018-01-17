;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit RemoveMantissa : 
  module RemoveMantissa : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<12><<4>>, out : Fixed<8><<0>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg reg : Fixed, clock @[FixedPrecisionChangerSpec.scala 35:16]
    reg <= io.in @[FixedPrecisionChangerSpec.scala 36:7]
    io.out <= reg @[FixedPrecisionChangerSpec.scala 37:10]
    
