;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit RealAdder : 
  extmodule BBFAdd : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  module RealAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a1 : {node : UInt<64>}, flip a2 : {node : UInt<64>}, c : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg register1 : {node : UInt<64>}, clock @[RealAdderSpec.scala 17:22]
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36]
    BBFAdd.out is invalid
    BBFAdd.in2 is invalid
    BBFAdd.in1 is invalid
    BBFAdd.in1 <= io.a1.node @[DspReal.scala 26:21]
    BBFAdd.in2 <= io.a2.node @[DspReal.scala 27:21]
    wire _T_16 : {node : UInt<64>} @[DspReal.scala 28:19]
    _T_16 is invalid @[DspReal.scala 28:19]
    _T_16.node <= BBFAdd.out @[DspReal.scala 29:14]
    register1.node <= _T_16.node @[RealAdderSpec.scala 19:13]
    io.c.node <= register1.node @[RealAdderSpec.scala 21:8]
    
