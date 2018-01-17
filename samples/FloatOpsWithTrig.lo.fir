circuit FloatOpsWithTrig : @[:@2.0]
  extmodule BBFAdd : @[:@3.2]
    output out : UInt<64> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFAdd
    

  extmodule BBFSubtract : @[:@11.2]
    output out : UInt<64> @[:@12.4]
    input in2 : UInt<64> @[:@13.4]
    input in1 : UInt<64> @[:@14.4]
  
    defname = BBFSubtract
    

  extmodule BBFMultiply : @[:@19.2]
    output out : UInt<64> @[:@20.4]
    input in2 : UInt<64> @[:@21.4]
    input in1 : UInt<64> @[:@22.4]
  
    defname = BBFMultiply
    

  extmodule BBFDivide : @[:@27.2]
    output out : UInt<64> @[:@28.4]
    input in2 : UInt<64> @[:@29.4]
    input in1 : UInt<64> @[:@30.4]
  
    defname = BBFDivide
    

  extmodule BBFLn : @[:@35.2]
    output out : UInt<64> @[:@36.4]
    input in : UInt<64> @[:@37.4]
  
    defname = BBFLn
    

  extmodule BBFLog10 : @[:@42.2]
    output out : UInt<64> @[:@43.4]
    input in : UInt<64> @[:@44.4]
  
    defname = BBFLog10
    

  extmodule BBFExp : @[:@49.2]
    output out : UInt<64> @[:@50.4]
    input in : UInt<64> @[:@51.4]
  
    defname = BBFExp
    

  extmodule BBFSqrt : @[:@56.2]
    output out : UInt<64> @[:@57.4]
    input in : UInt<64> @[:@58.4]
  
    defname = BBFSqrt
    

  extmodule BBFPow : @[:@63.2]
    output out : UInt<64> @[:@64.4]
    input in2 : UInt<64> @[:@65.4]
    input in1 : UInt<64> @[:@66.4]
  
    defname = BBFPow
    

  extmodule BBFFloor : @[:@71.2]
    output out : UInt<64> @[:@72.4]
    input in : UInt<64> @[:@73.4]
  
    defname = BBFFloor
    

  extmodule BBFCeil : @[:@78.2]
    output out : UInt<64> @[:@79.4]
    input in : UInt<64> @[:@80.4]
  
    defname = BBFCeil
    

  extmodule BBFGreaterThan : @[:@85.2]
    output out : UInt<1> @[:@86.4]
    input in2 : UInt<64> @[:@87.4]
    input in1 : UInt<64> @[:@88.4]
  
    defname = BBFGreaterThan
    

  extmodule BBFGreaterThanEquals : @[:@93.2]
    output out : UInt<1> @[:@94.4]
    input in2 : UInt<64> @[:@95.4]
    input in1 : UInt<64> @[:@96.4]
  
    defname = BBFGreaterThanEquals
    

  extmodule BBFSin : @[:@101.2]
    output out : UInt<64> @[:@102.4]
    input in : UInt<64> @[:@103.4]
  
    defname = BBFSin
    

  extmodule BBFCos : @[:@108.2]
    output out : UInt<64> @[:@109.4]
    input in : UInt<64> @[:@110.4]
  
    defname = BBFCos
    

  extmodule BBFTan : @[:@115.2]
    output out : UInt<64> @[:@116.4]
    input in : UInt<64> @[:@117.4]
  
    defname = BBFTan
    

  extmodule BBFASin : @[:@122.2]
    output out : UInt<64> @[:@123.4]
    input in : UInt<64> @[:@124.4]
  
    defname = BBFASin
    

  extmodule BBFACos : @[:@129.2]
    output out : UInt<64> @[:@130.4]
    input in : UInt<64> @[:@131.4]
  
    defname = BBFACos
    

  extmodule BBFATan : @[:@136.2]
    output out : UInt<64> @[:@137.4]
    input in : UInt<64> @[:@138.4]
  
    defname = BBFATan
    

  extmodule BBFATan2 : @[:@143.2]
    output out : UInt<64> @[:@144.4]
    input in2 : UInt<64> @[:@145.4]
    input in1 : UInt<64> @[:@146.4]
  
    defname = BBFATan2
    

  extmodule BBFHypot : @[:@151.2]
    output out : UInt<64> @[:@152.4]
    input in2 : UInt<64> @[:@153.4]
    input in1 : UInt<64> @[:@154.4]
  
    defname = BBFHypot
    

  extmodule BBFSinh : @[:@159.2]
    output out : UInt<64> @[:@160.4]
    input in : UInt<64> @[:@161.4]
  
    defname = BBFSinh
    

  extmodule BBFCosh : @[:@166.2]
    output out : UInt<64> @[:@167.4]
    input in : UInt<64> @[:@168.4]
  
    defname = BBFCosh
    

  extmodule BBFTanh : @[:@173.2]
    output out : UInt<64> @[:@174.4]
    input in : UInt<64> @[:@175.4]
  
    defname = BBFTanh
    

  extmodule BBFASinh : @[:@180.2]
    output out : UInt<64> @[:@181.4]
    input in : UInt<64> @[:@182.4]
  
    defname = BBFASinh
    

  extmodule BBFACosh : @[:@187.2]
    output out : UInt<64> @[:@188.4]
    input in : UInt<64> @[:@189.4]
  
    defname = BBFACosh
    

  extmodule BBFATanh : @[:@194.2]
    output out : UInt<64> @[:@195.4]
    input in : UInt<64> @[:@196.4]
  
    defname = BBFATanh
    

  module FloatOpsWithTrig : @[:@201.2]
    input clock : Clock @[:@202.4]
    input reset : UInt<1> @[:@203.4]
    input io_in1_node : UInt<64> @[:@204.4]
    input io_in2_node : UInt<64> @[:@204.4]
    input io_opsel : UInt<64> @[:@204.4]
    output io_out_node : UInt<64> @[:@204.4]
    output io_boolOut : UInt<1> @[:@204.4]
  
    node _T_11 = eq(UInt<1>("h0"), io_opsel) @[Conditional.scala 29:28:@209.4]
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36:@211.6]
    wire _T_14_node : UInt<64> @[DspReal.scala 28:19:@217.6]
    node _GEN_0 = validif(_T_11, _T_14_node) @[Conditional.scala 29:59:@210.4]
    node _T_17 = eq(UInt<1>("h1"), io_opsel) @[Conditional.scala 29:28:@222.4]
    inst BBFSubtract of BBFSubtract @[DspReal.scala 47:36:@224.6]
    wire _T_20_node : UInt<64> @[DspReal.scala 28:19:@230.6]
    node _GEN_1 = mux(_T_17, _T_20_node, _GEN_0) @[Conditional.scala 29:59:@223.4]
    node _T_23 = eq(UInt<2>("h2"), io_opsel) @[Conditional.scala 29:28:@235.4]
    inst BBFMultiply of BBFMultiply @[DspReal.scala 51:36:@237.6]
    wire _T_26_node : UInt<64> @[DspReal.scala 28:19:@243.6]
    node _GEN_2 = mux(_T_23, _T_26_node, _GEN_1) @[Conditional.scala 29:59:@236.4]
    node _T_29 = eq(UInt<2>("h3"), io_opsel) @[Conditional.scala 29:28:@248.4]
    inst BBFDivide of BBFDivide @[DspReal.scala 55:36:@250.6]
    wire _T_32_node : UInt<64> @[DspReal.scala 28:19:@256.6]
    node _GEN_3 = mux(_T_29, _T_32_node, _GEN_2) @[Conditional.scala 29:59:@249.4]
    node _T_35 = eq(UInt<3>("h4"), io_opsel) @[Conditional.scala 29:28:@261.4]
    inst BBFLn of BBFLn @[DspReal.scala 83:30:@263.6]
    wire _T_38_node : UInt<64> @[DspReal.scala 19:19:@267.6]
    node _GEN_4 = mux(_T_35, _T_38_node, _GEN_3) @[Conditional.scala 29:59:@262.4]
    node _T_41 = eq(UInt<3>("h5"), io_opsel) @[Conditional.scala 29:28:@272.4]
    inst BBFLog10 of BBFLog10 @[DspReal.scala 87:30:@274.6]
    wire _T_44_node : UInt<64> @[DspReal.scala 19:19:@278.6]
    node _GEN_5 = mux(_T_41, _T_44_node, _GEN_4) @[Conditional.scala 29:59:@273.4]
    node _T_47 = eq(UInt<3>("h6"), io_opsel) @[Conditional.scala 29:28:@283.4]
    inst BBFExp of BBFExp @[DspReal.scala 91:30:@285.6]
    wire _T_50_node : UInt<64> @[DspReal.scala 19:19:@289.6]
    node _GEN_6 = mux(_T_47, _T_50_node, _GEN_5) @[Conditional.scala 29:59:@284.4]
    node _T_53 = eq(UInt<3>("h7"), io_opsel) @[Conditional.scala 29:28:@294.4]
    inst BBFSqrt of BBFSqrt @[DspReal.scala 95:30:@296.6]
    wire _T_56_node : UInt<64> @[DspReal.scala 19:19:@300.6]
    node _GEN_7 = mux(_T_53, _T_56_node, _GEN_6) @[Conditional.scala 29:59:@295.4]
    node _T_59 = eq(UInt<4>("h8"), io_opsel) @[Conditional.scala 29:28:@305.4]
    inst BBFPow of BBFPow @[DspReal.scala 99:36:@307.6]
    wire _T_62_node : UInt<64> @[DspReal.scala 28:19:@313.6]
    node _GEN_8 = mux(_T_59, _T_62_node, _GEN_7) @[Conditional.scala 29:59:@306.4]
    node _T_65 = eq(UInt<4>("h9"), io_opsel) @[Conditional.scala 29:28:@318.4]
    inst BBFFloor of BBFFloor @[DspReal.scala 103:30:@320.6]
    wire _T_68_node : UInt<64> @[DspReal.scala 19:19:@324.6]
    node _GEN_9 = mux(_T_65, _T_68_node, _GEN_8) @[Conditional.scala 29:59:@319.4]
    node _T_71 = eq(UInt<4>("ha"), io_opsel) @[Conditional.scala 29:28:@329.4]
    inst BBFCeil of BBFCeil @[DspReal.scala 107:30:@331.6]
    wire _T_74_node : UInt<64> @[DspReal.scala 19:19:@335.6]
    node _GEN_10 = mux(_T_71, _T_74_node, _GEN_9) @[Conditional.scala 29:59:@330.4]
    node _T_77 = eq(UInt<5>("h19"), io_opsel) @[Conditional.scala 29:28:@340.4]
    inst BBFGreaterThan of BBFGreaterThan @[DspReal.scala 59:32:@342.6]
    wire _T_80 : UInt<1> @[DspReal.scala 37:19:@348.6]
    node _GEN_11 = validif(_T_77, _T_80) @[Conditional.scala 29:59:@341.4]
    node _T_82 = eq(UInt<5>("h1a"), io_opsel) @[Conditional.scala 29:28:@353.4]
    inst BBFGreaterThanEquals of BBFGreaterThanEquals @[DspReal.scala 63:32:@355.6]
    wire _T_85 : UInt<1> @[DspReal.scala 37:19:@361.6]
    node _GEN_12 = mux(_T_82, _T_85, _GEN_11) @[Conditional.scala 29:59:@354.4]
    node _T_87 = eq(UInt<4>("hb"), io_opsel) @[Conditional.scala 29:28:@366.4]
    inst BBFSin of BBFSin @[DspReal.scala 185:32:@368.6]
    wire _T_90_node : UInt<64> @[DspReal.scala 19:19:@372.6]
    node _GEN_13 = mux(_T_87, _T_90_node, _GEN_10) @[Conditional.scala 29:59:@367.4]
    node _T_93 = eq(UInt<4>("hc"), io_opsel) @[Conditional.scala 29:28:@377.4]
    inst BBFCos of BBFCos @[DspReal.scala 191:35:@379.6]
    wire _T_96_node : UInt<64> @[DspReal.scala 19:19:@383.6]
    node _GEN_14 = mux(_T_93, _T_96_node, _GEN_13) @[Conditional.scala 29:59:@378.4]
    node _T_99 = eq(UInt<4>("hd"), io_opsel) @[Conditional.scala 29:28:@388.4]
    inst BBFTan of BBFTan @[DspReal.scala 211:35:@390.6]
    wire _T_102_node : UInt<64> @[DspReal.scala 19:19:@394.6]
    node _GEN_15 = mux(_T_99, _T_102_node, _GEN_14) @[Conditional.scala 29:59:@389.4]
    node _T_105 = eq(UInt<4>("he"), io_opsel) @[Conditional.scala 29:28:@399.4]
    inst BBFASin of BBFASin @[DspReal.scala 249:35:@401.6]
    wire _T_108_node : UInt<64> @[DspReal.scala 19:19:@405.6]
    node _GEN_16 = mux(_T_105, _T_108_node, _GEN_15) @[Conditional.scala 29:59:@400.4]
    node _T_111 = eq(UInt<4>("hf"), io_opsel) @[Conditional.scala 29:28:@410.4]
    inst BBFACos of BBFACos @[DspReal.scala 257:35:@412.6]
    wire _T_114_node : UInt<64> @[DspReal.scala 19:19:@416.6]
    node _GEN_17 = mux(_T_111, _T_114_node, _GEN_16) @[Conditional.scala 29:59:@411.4]
    node _T_117 = eq(UInt<5>("h10"), io_opsel) @[Conditional.scala 29:28:@421.4]
    inst BBFATan of BBFATan @[DspReal.scala 237:35:@423.6]
    wire _T_120_node : UInt<64> @[DspReal.scala 19:19:@427.6]
    node _GEN_18 = mux(_T_117, _T_120_node, _GEN_17) @[Conditional.scala 29:59:@422.4]
    node _T_123 = eq(UInt<5>("h11"), io_opsel) @[Conditional.scala 29:28:@432.4]
    inst BBFATan2 of BBFATan2 @[DspReal.scala 278:41:@434.6]
    wire _T_126_node : UInt<64> @[DspReal.scala 28:19:@440.6]
    node _GEN_19 = mux(_T_123, _T_126_node, _GEN_18) @[Conditional.scala 29:59:@433.4]
    node _T_129 = eq(UInt<5>("h12"), io_opsel) @[Conditional.scala 29:28:@445.4]
    inst BBFHypot of BBFHypot @[DspReal.scala 283:41:@447.6]
    wire _T_132_node : UInt<64> @[DspReal.scala 28:19:@453.6]
    node _GEN_20 = mux(_T_129, _T_132_node, _GEN_19) @[Conditional.scala 29:59:@446.4]
    node _T_135 = eq(UInt<5>("h13"), io_opsel) @[Conditional.scala 29:28:@458.4]
    inst BBFSinh of BBFSinh @[DspReal.scala 290:35:@460.6]
    wire _T_138_node : UInt<64> @[DspReal.scala 19:19:@464.6]
    node _GEN_21 = mux(_T_135, _T_138_node, _GEN_20) @[Conditional.scala 29:59:@459.4]
    node _T_141 = eq(UInt<5>("h14"), io_opsel) @[Conditional.scala 29:28:@469.4]
    inst BBFCosh of BBFCosh @[DspReal.scala 295:35:@471.6]
    wire _T_144_node : UInt<64> @[DspReal.scala 19:19:@475.6]
    node _GEN_22 = mux(_T_141, _T_144_node, _GEN_21) @[Conditional.scala 29:59:@470.4]
    node _T_147 = eq(UInt<5>("h15"), io_opsel) @[Conditional.scala 29:28:@480.4]
    inst BBFTanh of BBFTanh @[DspReal.scala 300:35:@482.6]
    wire _T_150_node : UInt<64> @[DspReal.scala 19:19:@486.6]
    node _GEN_23 = mux(_T_147, _T_150_node, _GEN_22) @[Conditional.scala 29:59:@481.4]
    node _T_153 = eq(UInt<5>("h16"), io_opsel) @[Conditional.scala 29:28:@491.4]
    inst BBFASinh of BBFASinh @[DspReal.scala 307:35:@493.6]
    wire _T_156_node : UInt<64> @[DspReal.scala 19:19:@497.6]
    node _GEN_24 = mux(_T_153, _T_156_node, _GEN_23) @[Conditional.scala 29:59:@492.4]
    node _T_159 = eq(UInt<5>("h17"), io_opsel) @[Conditional.scala 29:28:@502.4]
    inst BBFACosh of BBFACosh @[DspReal.scala 313:35:@504.6]
    wire _T_162_node : UInt<64> @[DspReal.scala 19:19:@508.6]
    node _GEN_25 = mux(_T_159, _T_162_node, _GEN_24) @[Conditional.scala 29:59:@503.4]
    node _T_165 = eq(UInt<5>("h18"), io_opsel) @[Conditional.scala 29:28:@513.4]
    inst BBFATanh of BBFATanh @[DspReal.scala 319:35:@515.6]
    wire _T_168_node : UInt<64> @[DspReal.scala 19:19:@519.6]
    node _GEN_26 = mux(_T_165, _T_168_node, _GEN_25) @[Conditional.scala 29:59:@514.4]
    io_out_node <= _GEN_26
    io_boolOut <= _GEN_12
    BBFAdd.in2 <= io_in2_node
    BBFAdd.in1 <= io_in1_node
    _T_14_node <= BBFAdd.out
    BBFSubtract.in2 <= io_in2_node
    BBFSubtract.in1 <= io_in1_node
    _T_20_node <= BBFSubtract.out
    BBFMultiply.in2 <= io_in2_node
    BBFMultiply.in1 <= io_in1_node
    _T_26_node <= BBFMultiply.out
    BBFDivide.in2 <= io_in2_node
    BBFDivide.in1 <= io_in1_node
    _T_32_node <= BBFDivide.out
    BBFLn.in <= io_in1_node
    _T_38_node <= BBFLn.out
    BBFLog10.in <= io_in1_node
    _T_44_node <= BBFLog10.out
    BBFExp.in <= io_in1_node
    _T_50_node <= BBFExp.out
    BBFSqrt.in <= io_in1_node
    _T_56_node <= BBFSqrt.out
    BBFPow.in2 <= io_in2_node
    BBFPow.in1 <= io_in1_node
    _T_62_node <= BBFPow.out
    BBFFloor.in <= io_in1_node
    _T_68_node <= BBFFloor.out
    BBFCeil.in <= io_in1_node
    _T_74_node <= BBFCeil.out
    BBFGreaterThan.in2 <= io_in2_node
    BBFGreaterThan.in1 <= io_in1_node
    _T_80 <= BBFGreaterThan.out
    BBFGreaterThanEquals.in2 <= io_in2_node
    BBFGreaterThanEquals.in1 <= io_in1_node
    _T_85 <= BBFGreaterThanEquals.out
    BBFSin.in <= io_in1_node
    _T_90_node <= BBFSin.out
    BBFCos.in <= io_in1_node
    _T_96_node <= BBFCos.out
    BBFTan.in <= io_in1_node
    _T_102_node <= BBFTan.out
    BBFASin.in <= io_in1_node
    _T_108_node <= BBFASin.out
    BBFACos.in <= io_in1_node
    _T_114_node <= BBFACos.out
    BBFATan.in <= io_in1_node
    _T_120_node <= BBFATan.out
    BBFATan2.in2 <= io_in2_node
    BBFATan2.in1 <= io_in1_node
    _T_126_node <= BBFATan2.out
    BBFHypot.in2 <= io_in2_node
    BBFHypot.in1 <= io_in1_node
    _T_132_node <= BBFHypot.out
    BBFSinh.in <= io_in1_node
    _T_138_node <= BBFSinh.out
    BBFCosh.in <= io_in1_node
    _T_144_node <= BBFCosh.out
    BBFTanh.in <= io_in1_node
    _T_150_node <= BBFTanh.out
    BBFASinh.in <= io_in1_node
    _T_156_node <= BBFASinh.out
    BBFACosh.in <= io_in1_node
    _T_162_node <= BBFACosh.out
    BBFATanh.in <= io_in1_node
    _T_168_node <= BBFATanh.out
