;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit LnModule : 
  extmodule BBFLn : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFLn
    
    
  module LnModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip num : {node : UInt<64>}, ln : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst BBFLn of BBFLn @[DspReal.scala 83:30]
    BBFLn.out is invalid
    BBFLn.in is invalid
    BBFLn.in <= io.num.node @[DspReal.scala 18:20]
    wire _T_8 : {node : UInt<64>} @[DspReal.scala 19:19]
    _T_8 is invalid @[DspReal.scala 19:19]
    _T_8.node <= BBFLn.out @[DspReal.scala 20:14]
    io.ln.node <= _T_8.node @[LnSpec.scala 18:9]
    
