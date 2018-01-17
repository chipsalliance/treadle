circuit FixedRing1 : @[:@2.0]
  module FixedRing1 : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<16> @[:@6.4]
    output io_floor : SInt<16> @[:@6.4]
    output io_ceil : SInt<16> @[:@6.4]
    output io_isWhole : UInt<1> @[:@6.4]
    output io_round : SInt<16> @[:@6.4]
    output io_real_node : UInt<64> @[:@6.4]
  
    node _T_9 = shr(io_in, 4) @[FixedPointTypeClass.scala 69:58:@11.4]
    node _T_11 = shr(io_in, 4) @[FixedPointTypeClass.scala 69:58:@13.4]
    node _T_12 = eq(io_in, shl(_T_11, 4)) @[FixedPointTypeClass.scala 70:40:@14.4]
    node _T_14 = shr(io_in, 4) @[FixedPointTypeClass.scala 69:58:@15.4]
    node _T_15 = shr(io_in, 4) @[FixedPointTypeClass.scala 69:58:@16.4]
    node _T_17 = add(_T_15, asSInt(UInt<2>("h1"))) @[FixedPointTypeClass.scala 25:22:@17.4]
    node _T_20 = mux(_T_12, _T_14, _T_17) @[FixedPointTypeClass.scala 187:8:@18.4]
    node _T_21 = shr(io_in, 4) @[FixedPointTypeClass.scala 69:58:@20.4]
    node _T_22 = eq(io_in, shl(_T_21, 4)) @[FixedPointTypeClass.scala 70:40:@21.4]
    node _T_24 = add(io_in, shl(asSInt(UInt<2>("h1")), 3)) @[FixedPointTypeClass.scala 25:22:@23.4]
    node _T_26 = shr(_T_24, 4) @[FixedPointTypeClass.scala 69:58:@24.4]
    io_floor <= shl(_T_9, 4)
    io_ceil <= asSInt(bits(shl(_T_20, 4), 15, 0))
    io_isWhole <= _T_22
    io_round <= asSInt(bits(shl(_T_26, 4), 15, 0))
    io_real_node is invalid
