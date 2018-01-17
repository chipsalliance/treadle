circuit SignedModule : @[:@2.0]
  module SignedModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<10> @[:@6.4]
    output io_out : SInt<10> @[:@6.4]
  
    node _T_5 = eq(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 66:24:@11.4]
    node _T_7 = lt(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 66:35:@12.4]
    wire _T_10_eq : UInt<1> @[Comparison.scala 25:19:@13.4]
    wire _T_10_lt : UInt<1> @[Comparison.scala 25:19:@13.4]
    wire _T_12_zero : UInt<1> @[Sign.scala 54:73:@17.4]
    wire _T_12_neg : UInt<1> @[Sign.scala 54:73:@17.4]
    node _T_13 = bits(io_in, 9, 9) @[SIntTypeClass.scala 70:24:@21.4]
    node _T_15 = sub(asSInt(UInt<1>("h0")), io_in) @[SIntTypeClass.scala 28:50:@22.4]
    node _T_16 = tail(_T_15, 1) @[SIntTypeClass.scala 28:50:@23.4]
    node _T_17 = asSInt(_T_16) @[SIntTypeClass.scala 28:50:@24.4]
    node _T_18 = mux(_T_13, _T_17, io_in) @[SIntTypeClass.scala 140:31:@25.4]
    node _T_20 = eq(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 66:24:@26.4]
    node _T_22 = lt(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 66:35:@27.4]
    wire _T_25_eq : UInt<1> @[Comparison.scala 25:19:@28.4]
    wire _T_25_lt : UInt<1> @[Comparison.scala 25:19:@28.4]
    wire _T_27_zero : UInt<1> @[Sign.scala 54:73:@32.4]
    wire _T_27_neg : UInt<1> @[Sign.scala 54:73:@32.4]
    node _T_28 = mux(_T_27_zero, io_in, io_in) @[TypeclassSpec.scala 62:68:@36.4]
    node _T_29 = mux(_T_12_neg, _T_18, _T_28) @[TypeclassSpec.scala 62:21:@37.4]
    io_out <= _T_29
    _T_10_eq <= _T_5
    _T_10_lt <= _T_7
    _T_12_zero <= _T_10_eq
    _T_12_neg <= _T_10_lt
    _T_25_eq <= _T_20
    _T_25_lt <= _T_22
    _T_27_zero <= _T_25_eq
    _T_27_neg <= _T_25_lt
