;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit BinaryRepr : 
  module BinaryRepr : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip uIn : UInt<8>, flip sIn : SInt<8>, flip fIn : Fixed<10><<2>>, uOut : UInt<1>, sOut : UInt<1>, fOut : UInt<1>, uDiv2 : UInt<8>, sDiv2 : SInt<8>, fDiv2 : Fixed<10><<2>>, uMul2 : UInt<16>, sMul2 : SInt<16>, fMul2 : Fixed<20><<2>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.uOut <= UInt<1>("h00") @[NumbersSpec.scala 297:11]
    node _T_9 = bits(io.sIn, 7, 7) @[SIntTypeClass.scala 70:24]
    io.sOut <= _T_9 @[NumbersSpec.scala 298:11]
    node _T_10 = bits(io.fIn, 9, 9) @[FixedPointTypeClass.scala 181:24]
    io.fOut <= _T_10 @[NumbersSpec.scala 299:11]
    node _T_11 = shr(io.uIn, 2) @[UIntTypeClass.scala 125:38]
    io.uDiv2 <= _T_11 @[NumbersSpec.scala 301:12]
    node _T_12 = asFixedPoint(io.sIn, 0) @[SIntTypeClass.scala 114:61]
    wire _T_14 : Fixed<10><<2>> @[FixedPointTypeClass.scala 130:22]
    _T_14 is invalid @[FixedPointTypeClass.scala 130:22]
    _T_14 <= _T_12 @[FixedPointTypeClass.scala 131:12]
    wire _T_16 : Fixed<8><<2>> @[FixedPointTypeClass.scala 132:23]
    _T_16 is invalid @[FixedPointTypeClass.scala 132:23]
    node _T_17 = shr(_T_14, 2) @[FixedPointTypeClass.scala 134:23]
    _T_16 <= _T_17 @[FixedPointTypeClass.scala 134:13]
    node _T_18 = bpset(_T_16, 0) @[FixedPointTypeClass.scala 154:39]
    node _T_19 = asSInt(_T_18) @[SIntTypeClass.scala 157:29]
    io.sDiv2 <= _T_19 @[NumbersSpec.scala 302:12]
    wire _T_21 : Fixed<12><<4>> @[FixedPointTypeClass.scala 130:22]
    _T_21 is invalid @[FixedPointTypeClass.scala 130:22]
    _T_21 <= io.fIn @[FixedPointTypeClass.scala 131:12]
    wire _T_23 : Fixed<10><<4>> @[FixedPointTypeClass.scala 132:23]
    _T_23 is invalid @[FixedPointTypeClass.scala 132:23]
    node _T_24 = shr(_T_21, 2) @[FixedPointTypeClass.scala 134:23]
    _T_23 <= _T_24 @[FixedPointTypeClass.scala 134:13]
    node _T_25 = bpset(_T_23, 3) @[FixedPointTypeClass.scala 154:39]
    io.fDiv2 <= _T_25 @[NumbersSpec.scala 303:12]
    node _T_26 = shl(io.uIn, 2) @[UIntTypeClass.scala 123:38]
    io.uMul2 <= _T_26 @[NumbersSpec.scala 305:12]
    node _T_27 = shl(io.sIn, 2) @[SIntTypeClass.scala 121:38]
    io.sMul2 <= _T_27 @[NumbersSpec.scala 306:12]
    node _T_28 = shl(io.fIn, 2) @[FixedPointTypeClass.scala 114:50]
    io.fMul2 <= _T_28 @[NumbersSpec.scala 307:12]
    
