;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit CeilTruncateCircuitWithDelays : 
  extmodule BBFCeil : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFCeil
    
    
  extmodule BBFLessThan : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFLessThan
    
    
  extmodule BBFCeil_1 : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFCeil
    
    
  extmodule BBFFloor : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFFloor
    
    
  module CeilTruncateCircuitWithDelays : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inFixed : Fixed<12><<4>>, flip inReal : {node : UInt<64>}, outFixedCeil : Fixed<12><<4>>, outRealCeil : {node : UInt<64>}, outFixedTruncate : Fixed<12><<4>>, outRealTruncate : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg _T_16 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_16 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_18 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_18 <= _T_16 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_19 = bpset(_T_18, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_20 = eq(_T_18, _T_19) @[FixedPointTypeClass.scala 70:40]
    reg _T_23 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_23 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_25 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_25 <= _T_23 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_26 = bpset(_T_25, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_27 = bpset(io.inFixed, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_29 = add(_T_27, asFixedPoint(UInt<2>("h01"), 0)) @[FixedPointTypeClass.scala 25:22]
    reg _T_32 : Fixed<9><<0>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_32 <= _T_29 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_34 : Fixed<9><<0>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_34 <= _T_32 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_36 = mux(_T_20, _T_26, _T_34) @[FixedPointTypeClass.scala 187:8]
    io.outFixedCeil <= _T_36 @[ShiftRegisterDelaySpec.scala 42:21]
    reg _T_40 : {node : UInt<64>}, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_40.node <= io.inReal.node @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_44 : {node : UInt<64>}, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_44.node <= _T_40.node @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    inst BBFCeil of BBFCeil @[DspReal.scala 107:30]
    BBFCeil.out is invalid
    BBFCeil.in is invalid
    BBFCeil.in <= _T_44.node @[DspReal.scala 18:20]
    wire _T_48 : {node : UInt<64>} @[DspReal.scala 19:19]
    _T_48 is invalid @[DspReal.scala 19:19]
    _T_48.node <= BBFCeil.out @[DspReal.scala 20:14]
    io.outRealCeil.node <= _T_48.node @[ShiftRegisterDelaySpec.scala 43:20]
    reg _T_52 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_52 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_54 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_54 <= _T_52 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_55 = bits(_T_54, 11, 11) @[FixedPointTypeClass.scala 181:24]
    reg _T_58 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_58 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_60 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_60 <= _T_58 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_61 = bpset(_T_60, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_62 = eq(_T_60, _T_61) @[FixedPointTypeClass.scala 70:40]
    reg _T_65 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_65 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_67 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_67 <= _T_65 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_68 = bpset(_T_67, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_69 = bpset(io.inFixed, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_71 = add(_T_69, asFixedPoint(UInt<2>("h01"), 0)) @[FixedPointTypeClass.scala 25:22]
    reg _T_74 : Fixed<9><<0>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_74 <= _T_71 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_76 : Fixed<9><<0>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_76 <= _T_74 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_78 = mux(_T_62, _T_68, _T_76) @[FixedPointTypeClass.scala 187:8]
    reg _T_81 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_81 <= io.inFixed @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_83 : Fixed<12><<4>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_83 <= _T_81 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_84 = bpset(_T_83, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_86 = mux(_T_55, _T_78, _T_84) @[FixedPointTypeClass.scala 73:8]
    io.outFixedTruncate <= _T_86 @[ShiftRegisterDelaySpec.scala 44:25]
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32]
    BBFLessThan.out is invalid
    BBFLessThan.in2 is invalid
    BBFLessThan.in1 is invalid
    BBFLessThan.in1 <= io.inReal.node @[DspReal.scala 35:21]
    BBFLessThan.in2 <= UInt<64>("h00") @[DspReal.scala 36:21]
    wire _T_90 : UInt<1> @[DspReal.scala 37:19]
    _T_90 is invalid @[DspReal.scala 37:19]
    _T_90 <= BBFLessThan.out @[DspReal.scala 38:9]
    inst BBFCeil_1 of BBFCeil_1 @[DspReal.scala 107:30]
    BBFCeil_1.out is invalid
    BBFCeil_1.in is invalid
    BBFCeil_1.in <= io.inReal.node @[DspReal.scala 18:20]
    wire _T_93 : {node : UInt<64>} @[DspReal.scala 19:19]
    _T_93 is invalid @[DspReal.scala 19:19]
    _T_93.node <= BBFCeil_1.out @[DspReal.scala 20:14]
    inst BBFFloor of BBFFloor @[DspReal.scala 103:30]
    BBFFloor.out is invalid
    BBFFloor.in is invalid
    BBFFloor.in <= io.inReal.node @[DspReal.scala 18:20]
    wire _T_97 : {node : UInt<64>} @[DspReal.scala 19:19]
    _T_97 is invalid @[DspReal.scala 19:19]
    _T_97.node <= BBFFloor.out @[DspReal.scala 20:14]
    node _T_99 = mux(_T_90, _T_93, _T_97) @[DspReal.scala 113:8]
    io.outRealTruncate.node <= _T_99.node @[ShiftRegisterDelaySpec.scala 45:24]
    
