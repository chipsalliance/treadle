;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit NegCircuit : 
  extmodule BBFGreaterThan : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFGreaterThan
    
    
  module NegCircuit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : {node : UInt<64>}, flip in2 : {node : UInt<64>}, out : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst BBFGreaterThan of BBFGreaterThan @[DspReal.scala 59:32]
    BBFGreaterThan.out is invalid
    BBFGreaterThan.in2 is invalid
    BBFGreaterThan.in1 is invalid
    BBFGreaterThan.in1 <= io.in1.node @[DspReal.scala 35:21]
    BBFGreaterThan.in2 <= io.in2.node @[DspReal.scala 36:21]
    wire _T_11 : UInt<1> @[DspReal.scala 37:19]
    _T_11 is invalid @[DspReal.scala 37:19]
    _T_11 <= BBFGreaterThan.out @[DspReal.scala 38:9]
    io.out <= _T_11 @[BlackBoxFloat.scala 310:10]
    
