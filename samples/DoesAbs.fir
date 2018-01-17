;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit DoesAbs : 
  module DoesAbs : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip uIn : UInt<4>, flip sIn : SInt<4>, flip fIn : Fixed<5><<2>>, uAbsGrow : UInt<4>, uAbsWrap : UInt<4>, sAbsGrow : SInt<5>, sAbsWrap : SInt<4>, fAbsGrow : Fixed<6><<2>>, fAbsWrap : Fixed<5><<2>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.uAbsGrow <= io.uIn @[AbsSpec.scala 79:15]
    io.uAbsWrap <= io.uIn @[AbsSpec.scala 80:15]
    node _T_8 = geq(io.sIn, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 144:45]
    node _T_11 = sub(asSInt(UInt<1>("h00")), io.sIn) @[SIntTypeClass.scala 31:22]
    node _T_13 = mux(_T_8, io.sIn, _T_11) @[SIntTypeClass.scala 143:8]
    io.sAbsGrow <= _T_13 @[AbsSpec.scala 82:15]
    node _T_16 = geq(io.sIn, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 144:45]
    node _T_19 = sub(asSInt(UInt<1>("h00")), io.sIn) @[SIntTypeClass.scala 32:22]
    node _T_20 = tail(_T_19, 1) @[SIntTypeClass.scala 32:22]
    node _T_21 = asSInt(_T_20) @[SIntTypeClass.scala 32:22]
    node _T_23 = mux(_T_16, io.sIn, _T_21) @[SIntTypeClass.scala 143:8]
    io.sAbsWrap <= _T_23 @[AbsSpec.scala 83:15]
    node _T_25 = bits(io.fIn, 4, 4) @[FixedPointTypeClass.scala 181:24]
    node _T_27 = sub(asFixedPoint(UInt<1>("h00"), 0), io.fIn) @[FixedPointTypeClass.scala 34:22]
    node _T_31 = mux(_T_25, _T_27, io.fIn) @[FixedPointTypeClass.scala 207:8]
    io.fAbsGrow <= _T_31 @[AbsSpec.scala 85:15]
    node _T_33 = bits(io.fIn, 4, 4) @[FixedPointTypeClass.scala 181:24]
    node _T_35 = sub(asFixedPoint(UInt<1>("h00"), 0), io.fIn) @[FixedPointTypeClass.scala 35:22]
    node _T_36 = tail(_T_35, 1) @[FixedPointTypeClass.scala 35:22]
    node _T_37 = asFixedPoint(_T_36, 2) @[FixedPointTypeClass.scala 35:22]
    node _T_41 = mux(_T_33, _T_37, io.fIn) @[FixedPointTypeClass.scala 207:8]
    io.fAbsWrap <= _T_41 @[AbsSpec.scala 86:15]
    
