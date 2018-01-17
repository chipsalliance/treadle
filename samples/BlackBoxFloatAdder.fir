;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit BlackBoxFloatAdder : 
  extmodule BBFAdd : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  extmodule BBFAdd_1 : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  extmodule BBFAdd_2 : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  module BlackBoxFloatAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : {node : UInt<64>}, flip b : {node : UInt<64>}, c : {node : UInt<64>}, d : {node : UInt<64>}, e : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36]
    BBFAdd.out is invalid
    BBFAdd.in2 is invalid
    BBFAdd.in1 is invalid
    BBFAdd.in1 <= io.a.node @[DspReal.scala 26:21]
    BBFAdd.in2 <= io.b.node @[DspReal.scala 27:21]
    wire _T_14 : {node : UInt<64>} @[DspReal.scala 28:19]
    _T_14 is invalid @[DspReal.scala 28:19]
    _T_14.node <= BBFAdd.out @[DspReal.scala 29:14]
    io.c.node <= _T_14.node @[BlackBoxFloat.scala 54:8]
    inst BBFAdd_1 of BBFAdd_1 @[DspReal.scala 43:36]
    BBFAdd_1.out is invalid
    BBFAdd_1.in2 is invalid
    BBFAdd_1.in1 is invalid
    BBFAdd_1.in1 <= io.a.node @[DspReal.scala 26:21]
    BBFAdd_1.in2 <= io.a.node @[DspReal.scala 27:21]
    wire _T_18 : {node : UInt<64>} @[DspReal.scala 28:19]
    _T_18 is invalid @[DspReal.scala 28:19]
    _T_18.node <= BBFAdd_1.out @[DspReal.scala 29:14]
    io.d.node <= _T_18.node @[BlackBoxFloat.scala 55:8]
    inst BBFAdd_2 of BBFAdd_2 @[DspReal.scala 43:36]
    BBFAdd_2.out is invalid
    BBFAdd_2.in2 is invalid
    BBFAdd_2.in1 is invalid
    BBFAdd_2.in1 <= io.b.node @[DspReal.scala 26:21]
    BBFAdd_2.in2 <= io.b.node @[DspReal.scala 27:21]
    wire _T_22 : {node : UInt<64>} @[DspReal.scala 28:19]
    _T_22 is invalid @[DspReal.scala 28:19]
    _T_22.node <= BBFAdd_2.out @[DspReal.scala 29:14]
    io.e.node <= _T_22.node @[BlackBoxFloat.scala 56:8]
    
