;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit FloatOpsWithoutTrig : 
  extmodule BBFAdd : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  extmodule BBFSubtract : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFSubtract
    
    
  extmodule BBFMultiply : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFMultiply
    
    
  extmodule BBFDivide : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFDivide
    
    
  extmodule BBFLn : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFLn
    
    
  extmodule BBFLog10 : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFLog10
    
    
  extmodule BBFExp : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFExp
    
    
  extmodule BBFSqrt : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFSqrt
    
    
  extmodule BBFPow : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFPow
    
    
  extmodule BBFFloor : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFFloor
    
    
  extmodule BBFCeil : 
    output out : UInt<64>
    input in : UInt<64>
    
    defname = BBFCeil
    
    
  extmodule BBFGreaterThan : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFGreaterThan
    
    
  extmodule BBFGreaterThanEquals : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFGreaterThanEquals
    
    
  module FloatOpsWithoutTrig : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : {node : UInt<64>}, flip in2 : {node : UInt<64>}, flip opsel : UInt<64>, out : {node : UInt<64>}, boolOut : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_11 = eq(UInt<1>("h00"), io.opsel) @[Conditional.scala 29:28]
    when _T_11 : @[Conditional.scala 29:59]
      inst BBFAdd of BBFAdd @[DspReal.scala 43:36]
      BBFAdd.out is invalid
      BBFAdd.in2 is invalid
      BBFAdd.in1 is invalid
      BBFAdd.in1 <= io.in1.node @[DspReal.scala 26:21]
      BBFAdd.in2 <= io.in2.node @[DspReal.scala 27:21]
      wire _T_14 : {node : UInt<64>} @[DspReal.scala 28:19]
      _T_14 is invalid @[DspReal.scala 28:19]
      _T_14.node <= BBFAdd.out @[DspReal.scala 29:14]
      io.out.node <= _T_14.node @[BlackBoxFloat.scala 150:24]
      skip @[Conditional.scala 29:59]
    node _T_17 = eq(UInt<1>("h01"), io.opsel) @[Conditional.scala 29:28]
    when _T_17 : @[Conditional.scala 29:59]
      inst BBFSubtract of BBFSubtract @[DspReal.scala 47:36]
      BBFSubtract.out is invalid
      BBFSubtract.in2 is invalid
      BBFSubtract.in1 is invalid
      BBFSubtract.in1 <= io.in1.node @[DspReal.scala 26:21]
      BBFSubtract.in2 <= io.in2.node @[DspReal.scala 27:21]
      wire _T_20 : {node : UInt<64>} @[DspReal.scala 28:19]
      _T_20 is invalid @[DspReal.scala 28:19]
      _T_20.node <= BBFSubtract.out @[DspReal.scala 29:14]
      io.out.node <= _T_20.node @[BlackBoxFloat.scala 151:29]
      skip @[Conditional.scala 29:59]
    node _T_23 = eq(UInt<2>("h02"), io.opsel) @[Conditional.scala 29:28]
    when _T_23 : @[Conditional.scala 29:59]
      inst BBFMultiply of BBFMultiply @[DspReal.scala 51:36]
      BBFMultiply.out is invalid
      BBFMultiply.in2 is invalid
      BBFMultiply.in1 is invalid
      BBFMultiply.in1 <= io.in1.node @[DspReal.scala 26:21]
      BBFMultiply.in2 <= io.in2.node @[DspReal.scala 27:21]
      wire _T_26 : {node : UInt<64>} @[DspReal.scala 28:19]
      _T_26 is invalid @[DspReal.scala 28:19]
      _T_26.node <= BBFMultiply.out @[DspReal.scala 29:14]
      io.out.node <= _T_26.node @[BlackBoxFloat.scala 152:29]
      skip @[Conditional.scala 29:59]
    node _T_29 = eq(UInt<2>("h03"), io.opsel) @[Conditional.scala 29:28]
    when _T_29 : @[Conditional.scala 29:59]
      inst BBFDivide of BBFDivide @[DspReal.scala 55:36]
      BBFDivide.out is invalid
      BBFDivide.in2 is invalid
      BBFDivide.in1 is invalid
      BBFDivide.in1 <= io.in1.node @[DspReal.scala 26:21]
      BBFDivide.in2 <= io.in2.node @[DspReal.scala 27:21]
      wire _T_32 : {node : UInt<64>} @[DspReal.scala 28:19]
      _T_32 is invalid @[DspReal.scala 28:19]
      _T_32.node <= BBFDivide.out @[DspReal.scala 29:14]
      io.out.node <= _T_32.node @[BlackBoxFloat.scala 153:27]
      skip @[Conditional.scala 29:59]
    node _T_35 = eq(UInt<3>("h04"), io.opsel) @[Conditional.scala 29:28]
    when _T_35 : @[Conditional.scala 29:59]
      inst BBFLn of BBFLn @[DspReal.scala 83:30]
      BBFLn.out is invalid
      BBFLn.in is invalid
      BBFLn.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_38 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_38 is invalid @[DspReal.scala 19:19]
      _T_38.node <= BBFLn.out @[DspReal.scala 20:14]
      io.out.node <= _T_38.node @[BlackBoxFloat.scala 154:23]
      skip @[Conditional.scala 29:59]
    node _T_41 = eq(UInt<3>("h05"), io.opsel) @[Conditional.scala 29:28]
    when _T_41 : @[Conditional.scala 29:59]
      inst BBFLog10 of BBFLog10 @[DspReal.scala 87:30]
      BBFLog10.out is invalid
      BBFLog10.in is invalid
      BBFLog10.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_44 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_44 is invalid @[DspReal.scala 19:19]
      _T_44.node <= BBFLog10.out @[DspReal.scala 20:14]
      io.out.node <= _T_44.node @[BlackBoxFloat.scala 155:26]
      skip @[Conditional.scala 29:59]
    node _T_47 = eq(UInt<3>("h06"), io.opsel) @[Conditional.scala 29:28]
    when _T_47 : @[Conditional.scala 29:59]
      inst BBFExp of BBFExp @[DspReal.scala 91:30]
      BBFExp.out is invalid
      BBFExp.in is invalid
      BBFExp.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_50 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_50 is invalid @[DspReal.scala 19:19]
      _T_50.node <= BBFExp.out @[DspReal.scala 20:14]
      io.out.node <= _T_50.node @[BlackBoxFloat.scala 156:24]
      skip @[Conditional.scala 29:59]
    node _T_53 = eq(UInt<3>("h07"), io.opsel) @[Conditional.scala 29:28]
    when _T_53 : @[Conditional.scala 29:59]
      inst BBFSqrt of BBFSqrt @[DspReal.scala 95:30]
      BBFSqrt.out is invalid
      BBFSqrt.in is invalid
      BBFSqrt.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_56 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_56 is invalid @[DspReal.scala 19:19]
      _T_56.node <= BBFSqrt.out @[DspReal.scala 20:14]
      io.out.node <= _T_56.node @[BlackBoxFloat.scala 157:25]
      skip @[Conditional.scala 29:59]
    node _T_59 = eq(UInt<4>("h08"), io.opsel) @[Conditional.scala 29:28]
    when _T_59 : @[Conditional.scala 29:59]
      inst BBFPow of BBFPow @[DspReal.scala 99:36]
      BBFPow.out is invalid
      BBFPow.in2 is invalid
      BBFPow.in1 is invalid
      BBFPow.in1 <= io.in1.node @[DspReal.scala 26:21]
      BBFPow.in2 <= io.in2.node @[DspReal.scala 27:21]
      wire _T_62 : {node : UInt<64>} @[DspReal.scala 28:19]
      _T_62 is invalid @[DspReal.scala 28:19]
      _T_62.node <= BBFPow.out @[DspReal.scala 29:14]
      io.out.node <= _T_62.node @[BlackBoxFloat.scala 158:24]
      skip @[Conditional.scala 29:59]
    node _T_65 = eq(UInt<4>("h09"), io.opsel) @[Conditional.scala 29:28]
    when _T_65 : @[Conditional.scala 29:59]
      inst BBFFloor of BBFFloor @[DspReal.scala 103:30]
      BBFFloor.out is invalid
      BBFFloor.in is invalid
      BBFFloor.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_68 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_68 is invalid @[DspReal.scala 19:19]
      _T_68.node <= BBFFloor.out @[DspReal.scala 20:14]
      io.out.node <= _T_68.node @[BlackBoxFloat.scala 159:26]
      skip @[Conditional.scala 29:59]
    node _T_71 = eq(UInt<4>("h0a"), io.opsel) @[Conditional.scala 29:28]
    when _T_71 : @[Conditional.scala 29:59]
      inst BBFCeil of BBFCeil @[DspReal.scala 107:30]
      BBFCeil.out is invalid
      BBFCeil.in is invalid
      BBFCeil.in <= io.in1.node @[DspReal.scala 18:20]
      wire _T_74 : {node : UInt<64>} @[DspReal.scala 19:19]
      _T_74 is invalid @[DspReal.scala 19:19]
      _T_74.node <= BBFCeil.out @[DspReal.scala 20:14]
      io.out.node <= _T_74.node @[BlackBoxFloat.scala 160:25]
      skip @[Conditional.scala 29:59]
    node _T_77 = eq(UInt<5>("h019"), io.opsel) @[Conditional.scala 29:28]
    when _T_77 : @[Conditional.scala 29:59]
      inst BBFGreaterThan of BBFGreaterThan @[DspReal.scala 59:32]
      BBFGreaterThan.out is invalid
      BBFGreaterThan.in2 is invalid
      BBFGreaterThan.in1 is invalid
      BBFGreaterThan.in1 <= io.in1.node @[DspReal.scala 35:21]
      BBFGreaterThan.in2 <= io.in2.node @[DspReal.scala 36:21]
      wire _T_80 : UInt<1> @[DspReal.scala 37:19]
      _T_80 is invalid @[DspReal.scala 37:19]
      _T_80 <= BBFGreaterThan.out @[DspReal.scala 38:9]
      io.boolOut <= _T_80 @[BlackBoxFloat.scala 161:36]
      skip @[Conditional.scala 29:59]
    node _T_82 = eq(UInt<5>("h01a"), io.opsel) @[Conditional.scala 29:28]
    when _T_82 : @[Conditional.scala 29:59]
      inst BBFGreaterThanEquals of BBFGreaterThanEquals @[DspReal.scala 63:32]
      BBFGreaterThanEquals.out is invalid
      BBFGreaterThanEquals.in2 is invalid
      BBFGreaterThanEquals.in1 is invalid
      BBFGreaterThanEquals.in1 <= io.in1.node @[DspReal.scala 35:21]
      BBFGreaterThanEquals.in2 <= io.in2.node @[DspReal.scala 36:21]
      wire _T_85 : UInt<1> @[DspReal.scala 37:19]
      _T_85 is invalid @[DspReal.scala 37:19]
      _T_85 <= BBFGreaterThanEquals.out @[DspReal.scala 38:9]
      io.boolOut <= _T_85 @[BlackBoxFloat.scala 162:43]
      skip @[Conditional.scala 29:59]
    
