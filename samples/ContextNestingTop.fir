;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit ContextNestingTop : 
  module ContextNestingBottom : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : UInt<4>, flip in2 : UInt<4>, outDefault : UInt<5>, outWrap : UInt<5>, outGrow : UInt<5>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_2 = add(io.in1, io.in2) @[UIntTypeClass.scala 23:22]
    node _T_3 = tail(_T_2, 1) @[UIntTypeClass.scala 23:22]
    io.outWrap <= _T_3 @[DspContextSpec.scala 86:16]
    node _T_5 = add(io.in1, io.in2) @[UIntTypeClass.scala 22:22]
    io.outGrow <= _T_5 @[DspContextSpec.scala 89:16]
    node _T_7 = add(io.in1, io.in2) @[UIntTypeClass.scala 23:22]
    node _T_8 = tail(_T_7, 1) @[UIntTypeClass.scala 23:22]
    io.outDefault <= _T_8 @[DspContextSpec.scala 92:17]
    
  module ContextNestingBottom_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : UInt<4>, flip in2 : UInt<4>, outDefault : UInt<5>, outWrap : UInt<5>, outGrow : UInt<5>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_2 = add(io.in1, io.in2) @[UIntTypeClass.scala 23:22]
    node _T_3 = tail(_T_2, 1) @[UIntTypeClass.scala 23:22]
    io.outWrap <= _T_3 @[DspContextSpec.scala 86:16]
    node _T_5 = add(io.in1, io.in2) @[UIntTypeClass.scala 22:22]
    io.outGrow <= _T_5 @[DspContextSpec.scala 89:16]
    node _T_7 = add(io.in1, io.in2) @[UIntTypeClass.scala 22:22]
    io.outDefault <= _T_7 @[DspContextSpec.scala 92:17]
    
  module ContextNestingTop : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : UInt<4>, flip in2 : UInt<4>, mod1Default : UInt<5>, mod1Wrap : UInt<5>, mod1Grow : UInt<5>, mod2Default : UInt<5>, mod2Wrap : UInt<5>, mod2Grow : UInt<5>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst ContextNestingBottom of ContextNestingBottom @[DspContextSpec.scala 107:64]
    ContextNestingBottom.io is invalid
    ContextNestingBottom.clock <= clock
    ContextNestingBottom.reset <= reset
    inst ContextNestingBottom_1 of ContextNestingBottom_1 @[DspContextSpec.scala 108:64]
    ContextNestingBottom_1.io is invalid
    ContextNestingBottom_1.clock <= clock
    ContextNestingBottom_1.reset <= reset
    ContextNestingBottom.io.in1 <= io.in1 @[DspContextSpec.scala 110:15]
    ContextNestingBottom.io.in2 <= io.in2 @[DspContextSpec.scala 111:15]
    ContextNestingBottom_1.io.in1 <= io.in1 @[DspContextSpec.scala 112:15]
    ContextNestingBottom_1.io.in2 <= io.in2 @[DspContextSpec.scala 113:15]
    io.mod1Default <= ContextNestingBottom.io.outDefault @[DspContextSpec.scala 115:18]
    io.mod1Wrap <= ContextNestingBottom.io.outWrap @[DspContextSpec.scala 116:18]
    io.mod1Grow <= ContextNestingBottom.io.outGrow @[DspContextSpec.scala 117:18]
    io.mod2Default <= ContextNestingBottom_1.io.outDefault @[DspContextSpec.scala 118:18]
    io.mod2Wrap <= ContextNestingBottom_1.io.outWrap @[DspContextSpec.scala 119:18]
    io.mod2Grow <= ContextNestingBottom_1.io.outGrow @[DspContextSpec.scala 120:18]
    
