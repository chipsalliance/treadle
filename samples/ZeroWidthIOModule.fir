;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-25 20:31:59.290, builtAtMillis: 1506371519290
circuit ZeroWidthIOModule : 
  module ZeroWidthIOModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip zeroIn : UInt<0>, zeroOut : UInt<0>, flip otherIn : UInt<3>, otherOut : UInt<3>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.zeroOut <= io.zeroIn @[ZeroWidth.scala 15:15]
    io.otherOut <= io.otherIn @[ZeroWidth.scala 16:15]
    
