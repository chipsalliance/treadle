;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SimpleCaseClassModule : 
  module SimpleCaseClassModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {underlying : SInt<5>}, out : {underlying : SInt<5>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg register1 : {underlying : SInt<5>}, clock @[CaseClassBundleSpec.scala 27:22]
    register1.underlying <= io.in.underlying @[CaseClassBundleSpec.scala 29:13]
    io.out.underlying <= register1.underlying @[CaseClassBundleSpec.scala 31:10]
    
