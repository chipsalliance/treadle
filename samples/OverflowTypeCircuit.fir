;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit OverflowTypeCircuit : 
  module OverflowTypeCircuit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : SInt<4>, flip b : SInt<4>, addWrap : SInt<5>, addGrow : SInt<5>, subWrap : SInt<5>, subGrow : SInt<5>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_2 = add(io.a, io.b) @[SIntTypeClass.scala 23:22]
    node _T_3 = tail(_T_2, 1) @[SIntTypeClass.scala 23:22]
    node _T_4 = asSInt(_T_3) @[SIntTypeClass.scala 23:22]
    reg regAddWrap : SInt, clock @[NumbersSpec.scala 194:27]
    regAddWrap <= _T_4 @[NumbersSpec.scala 194:27]
    node _T_7 = add(io.a, io.b) @[SIntTypeClass.scala 22:22]
    reg regAddGrow : SInt, clock @[NumbersSpec.scala 195:27]
    regAddGrow <= _T_7 @[NumbersSpec.scala 195:27]
    node _T_10 = sub(io.a, io.b) @[SIntTypeClass.scala 32:22]
    node _T_11 = tail(_T_10, 1) @[SIntTypeClass.scala 32:22]
    node _T_12 = asSInt(_T_11) @[SIntTypeClass.scala 32:22]
    reg regSubWrap : SInt, clock @[NumbersSpec.scala 197:27]
    regSubWrap <= _T_12 @[NumbersSpec.scala 197:27]
    node _T_15 = sub(io.a, io.b) @[SIntTypeClass.scala 31:22]
    reg regSubGrow : SInt, clock @[NumbersSpec.scala 198:27]
    regSubGrow <= _T_15 @[NumbersSpec.scala 198:27]
    io.addWrap <= regAddWrap @[NumbersSpec.scala 201:14]
    io.addGrow <= regAddGrow @[NumbersSpec.scala 202:14]
    io.subWrap <= regSubWrap @[NumbersSpec.scala 203:14]
    io.subGrow <= regSubGrow @[NumbersSpec.scala 204:14]
    
