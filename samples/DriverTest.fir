;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit DriverTest : 
  module DriverTest : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<1>, out : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.out <= io.in @[DriverSpec.scala 15:10]
    
