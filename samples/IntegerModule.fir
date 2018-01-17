;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit IntegerModule : 
  module IntegerModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : SInt<10>, out : SInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_4 = rem(io.in, io.in) @[SIntTypeClass.scala 86:39]
    node _T_5 = add(io.in, _T_4) @[SIntTypeClass.scala 18:40]
    node _T_6 = tail(_T_5, 1) @[SIntTypeClass.scala 18:40]
    node _T_7 = asSInt(_T_6) @[SIntTypeClass.scala 18:40]
    io.out <= _T_7 @[TypeclassSpec.scala 25:10]
    
