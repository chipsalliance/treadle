;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-25 20:31:59.290, builtAtMillis: 1506371519290
circuit FixedPointReduce : 
  module FixedPointReduce : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<64><<60>>[10], sum : Fixed<64><<60>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_16 = add(io.in[0], io.in[1]) @[FixedPointSpec.scala 16:28]
    node _T_17 = tail(_T_16, 1) @[FixedPointSpec.scala 16:28]
    node _T_18 = asFixedPoint(_T_17, 60) @[FixedPointSpec.scala 16:28]
    node _T_19 = add(_T_18, io.in[2]) @[FixedPointSpec.scala 16:28]
    node _T_20 = tail(_T_19, 1) @[FixedPointSpec.scala 16:28]
    node _T_21 = asFixedPoint(_T_20, 60) @[FixedPointSpec.scala 16:28]
    node _T_22 = add(_T_21, io.in[3]) @[FixedPointSpec.scala 16:28]
    node _T_23 = tail(_T_22, 1) @[FixedPointSpec.scala 16:28]
    node _T_24 = asFixedPoint(_T_23, 60) @[FixedPointSpec.scala 16:28]
    node _T_25 = add(_T_24, io.in[4]) @[FixedPointSpec.scala 16:28]
    node _T_26 = tail(_T_25, 1) @[FixedPointSpec.scala 16:28]
    node _T_27 = asFixedPoint(_T_26, 60) @[FixedPointSpec.scala 16:28]
    node _T_28 = add(_T_27, io.in[5]) @[FixedPointSpec.scala 16:28]
    node _T_29 = tail(_T_28, 1) @[FixedPointSpec.scala 16:28]
    node _T_30 = asFixedPoint(_T_29, 60) @[FixedPointSpec.scala 16:28]
    node _T_31 = add(_T_30, io.in[6]) @[FixedPointSpec.scala 16:28]
    node _T_32 = tail(_T_31, 1) @[FixedPointSpec.scala 16:28]
    node _T_33 = asFixedPoint(_T_32, 60) @[FixedPointSpec.scala 16:28]
    node _T_34 = add(_T_33, io.in[7]) @[FixedPointSpec.scala 16:28]
    node _T_35 = tail(_T_34, 1) @[FixedPointSpec.scala 16:28]
    node _T_36 = asFixedPoint(_T_35, 60) @[FixedPointSpec.scala 16:28]
    node _T_37 = add(_T_36, io.in[8]) @[FixedPointSpec.scala 16:28]
    node _T_38 = tail(_T_37, 1) @[FixedPointSpec.scala 16:28]
    node _T_39 = asFixedPoint(_T_38, 60) @[FixedPointSpec.scala 16:28]
    node _T_40 = add(_T_39, io.in[9]) @[FixedPointSpec.scala 16:28]
    node _T_41 = tail(_T_40, 1) @[FixedPointSpec.scala 16:28]
    node _T_42 = asFixedPoint(_T_41, 60) @[FixedPointSpec.scala 16:28]
    io.sum <= _T_42 @[FixedPointSpec.scala 16:10]
    
