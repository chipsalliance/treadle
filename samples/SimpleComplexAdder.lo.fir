circuit SimpleComplexAdder : @[:@2.0]
  module SimpleComplexAdder : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a1_real : SInt<6> @[:@6.4]
    input io_a1_imag : SInt<6> @[:@6.4]
    input io_a2_real : SInt<8> @[:@6.4]
    input io_a2_imag : SInt<8> @[:@6.4]
    output io_c_real : SInt<14> @[:@6.4]
    output io_c_imag : SInt<14> @[:@6.4]
  
    reg register1_real : SInt<14>, clock with :
      reset => (UInt<1>("h0"), register1_real) @[ComplexAdderSpec.scala 23:22:@11.4]
    reg register1_imag : SInt<14>, clock with :
      reset => (UInt<1>("h0"), register1_imag) @[ComplexAdderSpec.scala 23:22:@11.4]
    node _T_28 = add(io_a2_real, io_a2_imag) @[FixedPointTypeClass.scala 21:58:@12.4]
    node _T_29 = tail(_T_28, 1) @[FixedPointTypeClass.scala 21:58:@13.4]
    node _T_30 = asSInt(_T_29) @[FixedPointTypeClass.scala 21:58:@14.4]
    node _T_31 = add(io_a1_real, io_a1_imag) @[FixedPointTypeClass.scala 21:58:@15.4]
    node _T_32 = tail(_T_31, 1) @[FixedPointTypeClass.scala 21:58:@16.4]
    node _T_33 = asSInt(_T_32) @[FixedPointTypeClass.scala 21:58:@17.4]
    node _T_34 = sub(io_a1_imag, io_a1_real) @[FixedPointTypeClass.scala 31:68:@18.4]
    node _T_35 = tail(_T_34, 1) @[FixedPointTypeClass.scala 31:68:@19.4]
    node _T_36 = asSInt(_T_35) @[FixedPointTypeClass.scala 31:68:@20.4]
    node _T_37 = mul(io_a1_real, _T_30) @[FixedPointTypeClass.scala 43:59:@21.4]
    node _T_38 = mul(_T_33, io_a2_imag) @[FixedPointTypeClass.scala 43:59:@22.4]
    node _T_39 = mul(_T_36, io_a2_real) @[FixedPointTypeClass.scala 43:59:@23.4]
    node _T_40 = sub(_T_37, _T_38) @[FixedPointTypeClass.scala 31:68:@24.4]
    node _T_41 = tail(_T_40, 1) @[FixedPointTypeClass.scala 31:68:@25.4]
    node _T_42 = asSInt(_T_41) @[FixedPointTypeClass.scala 31:68:@26.4]
    node _T_43 = add(_T_37, _T_39) @[FixedPointTypeClass.scala 21:58:@27.4]
    node _T_44 = tail(_T_43, 1) @[FixedPointTypeClass.scala 21:58:@28.4]
    node _T_45 = asSInt(_T_44) @[FixedPointTypeClass.scala 21:58:@29.4]
    wire _T_53_real : SInt<14> @[DspComplex.scala 30:22:@30.4]
    wire _T_53_imag : SInt<14> @[DspComplex.scala 30:22:@30.4]
    io_c_real <= register1_real
    io_c_imag <= register1_imag
    register1_real <= _T_53_real
    register1_imag <= _T_53_imag
    _T_53_real <= _T_42
    _T_53_imag <= _T_45
