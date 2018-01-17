circuit DoesAbs : @[:@2.0]
  module DoesAbs : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_uIn : UInt<4> @[:@6.4]
    input io_sIn : SInt<4> @[:@6.4]
    input io_fIn : SInt<5> @[:@6.4]
    output io_uAbsGrow : UInt<4> @[:@6.4]
    output io_uAbsWrap : UInt<4> @[:@6.4]
    output io_sAbsGrow : SInt<5> @[:@6.4]
    output io_sAbsWrap : SInt<4> @[:@6.4]
    output io_fAbsGrow : SInt<6> @[:@6.4]
    output io_fAbsWrap : SInt<5> @[:@6.4]
  
    node _T_8 = geq(io_sIn, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 144:45:@13.4]
    node _T_11 = sub(asSInt(UInt<1>("h0")), io_sIn) @[SIntTypeClass.scala 31:22:@14.4]
    node _T_13 = mux(_T_8, io_sIn, _T_11) @[SIntTypeClass.scala 143:8:@15.4]
    node _T_16 = geq(io_sIn, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 144:45:@17.4]
    node _T_19 = sub(asSInt(UInt<1>("h0")), io_sIn) @[SIntTypeClass.scala 32:22:@18.4]
    node _T_20 = tail(_T_19, 1) @[SIntTypeClass.scala 32:22:@19.4]
    node _T_21 = asSInt(_T_20) @[SIntTypeClass.scala 32:22:@20.4]
    node _T_23 = mux(_T_16, io_sIn, _T_21) @[SIntTypeClass.scala 143:8:@21.4]
    node _T_25 = bits(io_fIn, 4, 4) @[FixedPointTypeClass.scala 181:24:@23.4]
    node _T_27 = sub(shl(asSInt(UInt<1>("h0")), 2), io_fIn) @[FixedPointTypeClass.scala 34:22:@24.4]
    node _T_31 = mux(_T_25, _T_27, io_fIn) @[FixedPointTypeClass.scala 207:8:@25.4]
    node _T_33 = bits(io_fIn, 4, 4) @[FixedPointTypeClass.scala 181:24:@27.4]
    node _T_35 = sub(shl(asSInt(UInt<1>("h0")), 2), io_fIn) @[FixedPointTypeClass.scala 35:22:@28.4]
    node _T_36 = tail(_T_35, 1) @[FixedPointTypeClass.scala 35:22:@29.4]
    node _T_37 = asSInt(_T_36) @[FixedPointTypeClass.scala 35:22:@30.4]
    node _T_41 = mux(_T_33, _T_37, io_fIn) @[FixedPointTypeClass.scala 207:8:@31.4]
    io_uAbsGrow <= io_uIn
    io_uAbsWrap <= io_uIn
    io_sAbsGrow <= _T_13
    io_sAbsWrap <= _T_23
    io_fAbsGrow <= _T_31
    io_fAbsWrap <= _T_41
