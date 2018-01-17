circuit CeilTruncateCircuitWithDelays : @[:@2.0]
  extmodule BBFCeil : @[:@3.2]
    output out : UInt<64> @[:@4.4]
    input in : UInt<64> @[:@5.4]
  
    defname = BBFCeil
    

  extmodule BBFLessThan : @[:@10.2]
    output out : UInt<1> @[:@11.4]
    input in2 : UInt<64> @[:@12.4]
    input in1 : UInt<64> @[:@13.4]
  
    defname = BBFLessThan
    

  extmodule BBFFloor : @[:@25.2]
    output out : UInt<64> @[:@26.4]
    input in : UInt<64> @[:@27.4]
  
    defname = BBFFloor
    

  module CeilTruncateCircuitWithDelays : @[:@32.2]
    input clock : Clock @[:@33.4]
    input reset : UInt<1> @[:@34.4]
    input io_inFixed : SInt<12> @[:@35.4]
    input io_inReal_node : UInt<64> @[:@35.4]
    output io_outFixedCeil : SInt<12> @[:@35.4]
    output io_outRealCeil_node : UInt<64> @[:@35.4]
    output io_outFixedTruncate : SInt<12> @[:@35.4]
    output io_outRealTruncate_node : UInt<64> @[:@35.4]
  
    reg _T_16 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_16) @[Reg.scala 12:16:@40.4]
    node _GEN_0 = mux(UInt<1>("h1"), io_inFixed, _T_16) @[Reg.scala 13:19:@41.4]
    reg _T_18 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_18) @[Reg.scala 12:16:@44.4]
    node _GEN_1 = mux(UInt<1>("h1"), _T_16, _T_18) @[Reg.scala 13:19:@45.4]
    node _T_19 = shr(_T_18, 4) @[FixedPointTypeClass.scala 69:58:@48.4]
    node _T_20 = eq(_T_18, shl(_T_19, 4)) @[FixedPointTypeClass.scala 70:40:@49.4]
    reg _T_23 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_23) @[Reg.scala 12:16:@50.4]
    node _GEN_2 = mux(UInt<1>("h1"), io_inFixed, _T_23) @[Reg.scala 13:19:@51.4]
    reg _T_25 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_25) @[Reg.scala 12:16:@54.4]
    node _GEN_3 = mux(UInt<1>("h1"), _T_23, _T_25) @[Reg.scala 13:19:@55.4]
    node _T_26 = shr(_T_25, 4) @[FixedPointTypeClass.scala 69:58:@58.4]
    node _T_27 = shr(io_inFixed, 4) @[FixedPointTypeClass.scala 69:58:@59.4]
    node _T_29 = add(_T_27, asSInt(UInt<2>("h1"))) @[FixedPointTypeClass.scala 25:22:@60.4]
    reg _T_32 : SInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_32) @[Reg.scala 12:16:@61.4]
    node _GEN_4 = mux(UInt<1>("h1"), _T_29, _T_32) @[Reg.scala 13:19:@62.4]
    reg _T_34 : SInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_34) @[Reg.scala 12:16:@65.4]
    node _GEN_5 = mux(UInt<1>("h1"), _T_32, _T_34) @[Reg.scala 13:19:@66.4]
    node _T_36 = mux(_T_20, _T_26, _T_34) @[FixedPointTypeClass.scala 187:8:@69.4]
    reg _T_40_node : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_40_node) @[Reg.scala 12:16:@71.4]
    node _GEN_6 = mux(UInt<1>("h1"), io_inReal_node, _T_40_node) @[Reg.scala 13:19:@72.4]
    reg _T_44_node : UInt<64>, clock with :
      reset => (UInt<1>("h0"), _T_44_node) @[Reg.scala 12:16:@75.4]
    node _GEN_7 = mux(UInt<1>("h1"), _T_40_node, _T_44_node) @[Reg.scala 13:19:@76.4]
    inst BBFCeil of BBFCeil @[DspReal.scala 107:30:@79.4]
    wire _T_48_node : UInt<64> @[DspReal.scala 19:19:@83.4]
    reg _T_52 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_52) @[Reg.scala 12:16:@87.4]
    node _GEN_8 = mux(UInt<1>("h1"), io_inFixed, _T_52) @[Reg.scala 13:19:@88.4]
    reg _T_54 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_54) @[Reg.scala 12:16:@91.4]
    node _GEN_9 = mux(UInt<1>("h1"), _T_52, _T_54) @[Reg.scala 13:19:@92.4]
    node _T_55 = bits(_T_54, 11, 11) @[FixedPointTypeClass.scala 181:24:@95.4]
    reg _T_58 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_58) @[Reg.scala 12:16:@96.4]
    node _GEN_10 = mux(UInt<1>("h1"), io_inFixed, _T_58) @[Reg.scala 13:19:@97.4]
    reg _T_60 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_60) @[Reg.scala 12:16:@100.4]
    node _GEN_11 = mux(UInt<1>("h1"), _T_58, _T_60) @[Reg.scala 13:19:@101.4]
    node _T_61 = shr(_T_60, 4) @[FixedPointTypeClass.scala 69:58:@104.4]
    node _T_62 = eq(_T_60, shl(_T_61, 4)) @[FixedPointTypeClass.scala 70:40:@105.4]
    reg _T_65 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_65) @[Reg.scala 12:16:@106.4]
    node _GEN_12 = mux(UInt<1>("h1"), io_inFixed, _T_65) @[Reg.scala 13:19:@107.4]
    reg _T_67 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_67) @[Reg.scala 12:16:@110.4]
    node _GEN_13 = mux(UInt<1>("h1"), _T_65, _T_67) @[Reg.scala 13:19:@111.4]
    node _T_68 = shr(_T_67, 4) @[FixedPointTypeClass.scala 69:58:@114.4]
    node _T_69 = shr(io_inFixed, 4) @[FixedPointTypeClass.scala 69:58:@115.4]
    node _T_71 = add(_T_69, asSInt(UInt<2>("h1"))) @[FixedPointTypeClass.scala 25:22:@116.4]
    reg _T_74 : SInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_74) @[Reg.scala 12:16:@117.4]
    node _GEN_14 = mux(UInt<1>("h1"), _T_71, _T_74) @[Reg.scala 13:19:@118.4]
    reg _T_76 : SInt<9>, clock with :
      reset => (UInt<1>("h0"), _T_76) @[Reg.scala 12:16:@121.4]
    node _GEN_15 = mux(UInt<1>("h1"), _T_74, _T_76) @[Reg.scala 13:19:@122.4]
    node _T_78 = mux(_T_62, _T_68, _T_76) @[FixedPointTypeClass.scala 187:8:@125.4]
    reg _T_81 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_81) @[Reg.scala 12:16:@126.4]
    node _GEN_16 = mux(UInt<1>("h1"), io_inFixed, _T_81) @[Reg.scala 13:19:@127.4]
    reg _T_83 : SInt<12>, clock with :
      reset => (UInt<1>("h0"), _T_83) @[Reg.scala 12:16:@130.4]
    node _GEN_17 = mux(UInt<1>("h1"), _T_81, _T_83) @[Reg.scala 13:19:@131.4]
    node _T_84 = shr(_T_83, 4) @[FixedPointTypeClass.scala 69:58:@134.4]
    node _T_86 = mux(_T_55, _T_78, _T_84) @[FixedPointTypeClass.scala 73:8:@135.4]
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32:@137.4]
    wire _T_90 : UInt<1> @[DspReal.scala 37:19:@143.4]
    inst BBFCeil_1 of BBFCeil @[DspReal.scala 107:30:@146.4]
    wire _T_93_node : UInt<64> @[DspReal.scala 19:19:@150.4]
    inst BBFFloor of BBFFloor @[DspReal.scala 103:30:@153.4]
    wire _T_97_node : UInt<64> @[DspReal.scala 19:19:@157.4]
    node _T_99_node = mux(_T_90, _T_93_node, _T_97_node) @[DspReal.scala 113:8:@160.4]
    io_outFixedCeil <= asSInt(bits(shl(_T_36, 4), 11, 0))
    io_outRealCeil_node <= _T_48_node
    io_outFixedTruncate <= asSInt(bits(shl(_T_86, 4), 11, 0))
    io_outRealTruncate_node <= _T_99_node
    _T_16 <= _GEN_0
    _T_18 <= _GEN_1
    _T_23 <= _GEN_2
    _T_25 <= _GEN_3
    _T_32 <= _GEN_4
    _T_34 <= _GEN_5
    _T_40_node <= _GEN_6
    _T_44_node <= _GEN_7
    BBFCeil.in <= _T_44_node
    _T_48_node <= BBFCeil.out
    _T_52 <= _GEN_8
    _T_54 <= _GEN_9
    _T_58 <= _GEN_10
    _T_60 <= _GEN_11
    _T_65 <= _GEN_12
    _T_67 <= _GEN_13
    _T_74 <= _GEN_14
    _T_76 <= _GEN_15
    _T_81 <= _GEN_16
    _T_83 <= _GEN_17
    BBFLessThan.in2 <= UInt<64>("h0")
    BBFLessThan.in1 <= io_inReal_node
    _T_90 <= BBFLessThan.out
    BBFCeil_1.in <= io_inReal_node
    _T_93_node <= BBFCeil_1.out
    BBFFloor.in <= io_inReal_node
    _T_97_node <= BBFFloor.out
