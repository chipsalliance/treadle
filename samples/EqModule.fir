;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit EqModule : 
  module EqModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : SInt<10>, out : SInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = eq(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 55:48]
    node _T_7 = mux(_T_5, asSInt(UInt<2>("h01")), io.in) @[TypeclassSpec.scala 38:51]
    io.out <= _T_7 @[TypeclassSpec.scala 25:10]
    
