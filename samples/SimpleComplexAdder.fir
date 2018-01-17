;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SimpleComplexAdder : 
  module SimpleComplexAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a1 : {real : Fixed<6><<4>>, imag : Fixed<6><<4>>}, flip a2 : {real : Fixed<8><<1>>, imag : Fixed<8><<1>>}, c : {real : Fixed<14><<5>>, imag : Fixed<14><<5>>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg register1 : {real : Fixed<14><<5>>, imag : Fixed<14><<5>>}, clock @[ComplexAdderSpec.scala 23:22]
    node _T_28 = add(io.a2.real, io.a2.imag) @[FixedPointTypeClass.scala 21:58]
    node _T_29 = tail(_T_28, 1) @[FixedPointTypeClass.scala 21:58]
    node _T_30 = asFixedPoint(_T_29, 1) @[FixedPointTypeClass.scala 21:58]
    node _T_31 = add(io.a1.real, io.a1.imag) @[FixedPointTypeClass.scala 21:58]
    node _T_32 = tail(_T_31, 1) @[FixedPointTypeClass.scala 21:58]
    node _T_33 = asFixedPoint(_T_32, 4) @[FixedPointTypeClass.scala 21:58]
    node _T_34 = sub(io.a1.imag, io.a1.real) @[FixedPointTypeClass.scala 31:68]
    node _T_35 = tail(_T_34, 1) @[FixedPointTypeClass.scala 31:68]
    node _T_36 = asFixedPoint(_T_35, 4) @[FixedPointTypeClass.scala 31:68]
    node _T_37 = mul(io.a1.real, _T_30) @[FixedPointTypeClass.scala 43:59]
    node _T_38 = mul(_T_33, io.a2.imag) @[FixedPointTypeClass.scala 43:59]
    node _T_39 = mul(_T_36, io.a2.real) @[FixedPointTypeClass.scala 43:59]
    node _T_40 = sub(_T_37, _T_38) @[FixedPointTypeClass.scala 31:68]
    node _T_41 = tail(_T_40, 1) @[FixedPointTypeClass.scala 31:68]
    node _T_42 = asFixedPoint(_T_41, 5) @[FixedPointTypeClass.scala 31:68]
    node _T_43 = add(_T_37, _T_39) @[FixedPointTypeClass.scala 21:58]
    node _T_44 = tail(_T_43, 1) @[FixedPointTypeClass.scala 21:58]
    node _T_45 = asFixedPoint(_T_44, 5) @[FixedPointTypeClass.scala 21:58]
    wire _T_53 : {real : Fixed<14><<5>>, imag : Fixed<14><<5>>} @[DspComplex.scala 30:22]
    _T_53 is invalid @[DspComplex.scala 30:22]
    _T_53.real <= _T_42 @[DspComplex.scala 31:17]
    _T_53.imag <= _T_45 @[DspComplex.scala 32:17]
    register1.imag <= _T_53.imag @[ComplexAdderSpec.scala 28:13]
    register1.real <= _T_53.real @[ComplexAdderSpec.scala 28:13]
    io.c.imag <= register1.imag @[ComplexAdderSpec.scala 30:8]
    io.c.real <= register1.real @[ComplexAdderSpec.scala 30:8]
    
