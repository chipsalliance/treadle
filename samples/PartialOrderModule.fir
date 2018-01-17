;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit PartialOrderModule : 
  extmodule BBFEquals : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFLessThan : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFLessThan
    
    
  module PartialOrderModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {node : UInt<64>}, out : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst BBFEquals of BBFEquals @[DspReal.scala 75:32]
    BBFEquals.out is invalid
    BBFEquals.in2 is invalid
    BBFEquals.in1 is invalid
    BBFEquals.in1 <= io.in.node @[DspReal.scala 35:21]
    BBFEquals.in2 <= io.in.node @[DspReal.scala 36:21]
    wire _T_10 : UInt<1> @[DspReal.scala 37:19]
    _T_10 is invalid @[DspReal.scala 37:19]
    _T_10 <= BBFEquals.out @[DspReal.scala 38:9]
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32]
    BBFLessThan.out is invalid
    BBFLessThan.in2 is invalid
    BBFLessThan.in1 is invalid
    BBFLessThan.in1 <= io.in.node @[DspReal.scala 35:21]
    BBFLessThan.in2 <= io.in.node @[DspReal.scala 36:21]
    wire _T_13 : UInt<1> @[DspReal.scala 37:19]
    _T_13 is invalid @[DspReal.scala 37:19]
    _T_13 <= BBFLessThan.out @[DspReal.scala 38:9]
    wire _T_16 : {eq : UInt<1>, lt : UInt<1>} @[Comparison.scala 25:19]
    _T_16 is invalid @[Comparison.scala 25:19]
    _T_16.eq <= _T_10 @[Comparison.scala 26:12]
    _T_16.lt <= _T_13 @[Comparison.scala 27:12]
    wire _T_23 : {valid : UInt<1>, bits : {eq : UInt<1>, lt : UInt<1>}} @[Comparison.scala 18:19]
    _T_23 is invalid @[Comparison.scala 18:19]
    _T_23.bits.eq <= _T_16.eq @[Comparison.scala 19:17]
    _T_23.bits.lt <= _T_16.lt @[Comparison.scala 20:17]
    _T_23.valid <= UInt<1>("h01") @[Comparison.scala 21:15]
    node _T_26 = mux(_T_23.bits.eq, io.in, io.in) @[TypeclassSpec.scala 56:8]
    io.out.node <= _T_26.node @[TypeclassSpec.scala 25:10]
    
