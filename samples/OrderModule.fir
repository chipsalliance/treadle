;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit OrderModule : 
  extmodule BBFLessThan : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFLessThan
    
    
  module OrderModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {node : UInt<64>}, out : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32]
    BBFLessThan.out is invalid
    BBFLessThan.in2 is invalid
    BBFLessThan.in1 is invalid
    BBFLessThan.in1 <= io.in.node @[DspReal.scala 35:21]
    BBFLessThan.in2 <= io.in.node @[DspReal.scala 36:21]
    wire _T_10 : UInt<1> @[DspReal.scala 37:19]
    _T_10 is invalid @[DspReal.scala 37:19]
    _T_10 <= BBFLessThan.out @[DspReal.scala 38:9]
    node _T_11 = mux(_T_10, io.in, io.in) @[Order.scala 55:31]
    io.out.node <= _T_11.node @[TypeclassSpec.scala 25:10]
    
