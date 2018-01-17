;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit FixedPrecisionChanger : 
  module FixedPrecisionChanger : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<64><<58>>, out : Fixed<64><<16>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg reg : Fixed, clock @[FixedPrecisionChangerSpec.scala 18:16]
    reg <= io.in @[FixedPrecisionChangerSpec.scala 19:7]
    io.out <= reg @[FixedPrecisionChangerSpec.scala 20:10]
    
