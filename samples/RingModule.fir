;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit RingModule : 
  module RingModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : SInt<10>, out : SInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_6 = add(io.in, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 18:40]
    node _T_7 = tail(_T_6, 1) @[SIntTypeClass.scala 18:40]
    node _T_8 = asSInt(_T_7) @[SIntTypeClass.scala 18:40]
    node _T_9 = mul(asSInt(UInt<2>("h01")), _T_8) @[SIntTypeClass.scala 44:41]
    io.out <= _T_9 @[TypeclassSpec.scala 25:10]
    
