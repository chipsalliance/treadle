circuit RealGCD3 : @[:@2.0]
  module RealGCD3 : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    output io_in_ready : UInt<1> @[:@6.4]
    input io_in_valid : UInt<1> @[:@6.4]
    input io_in_bits_a : UInt<16> @[:@6.4]
    input io_in_bits_b : UInt<16> @[:@6.4]
    output io_out_valid : UInt<1> @[:@6.4]
    output io_out_bits : UInt<16> @[:@6.4]
  
    reg x : UInt<16>, clock with :
      reset => (UInt<1>("h0"), x) @[AdvTesterSpec.scala 40:14:@11.4]
    reg y : UInt<16>, clock with :
      reset => (UInt<1>("h0"), y) @[AdvTesterSpec.scala 41:14:@12.4]
    reg p : UInt<1>, clock with :
      reset => (UInt<1>("h0"), p) @[AdvTesterSpec.scala 42:18:@13.4]
    reg ti : UInt<16>, clock with :
      reset => (UInt<1>("h0"), ti) @[AdvTesterSpec.scala 44:19:@14.4]
    node _T_20 = add(ti, UInt<1>("h1")) @[AdvTesterSpec.scala 45:12:@15.4]
    node _T_21 = tail(_T_20, 1) @[AdvTesterSpec.scala 45:12:@16.4]
    node _T_23 = eq(p, UInt<1>("h0")) @[AdvTesterSpec.scala 47:18:@18.4]
    node _T_25 = eq(p, UInt<1>("h0")) @[AdvTesterSpec.scala 49:24:@20.4]
    node _T_26 = and(io_in_valid, _T_25) @[AdvTesterSpec.scala 49:21:@21.4]
    node _GEN_0 = mux(_T_26, io_in_bits_a, x) @[AdvTesterSpec.scala 49:28:@22.4]
    node _GEN_1 = mux(_T_26, io_in_bits_b, y) @[AdvTesterSpec.scala 49:28:@22.4]
    node _GEN_2 = mux(_T_26, UInt<1>("h1"), p) @[AdvTesterSpec.scala 49:28:@22.4]
    node _T_28 = gt(x, y) @[AdvTesterSpec.scala 56:13:@28.6]
    node _T_29 = sub(y, x) @[AdvTesterSpec.scala 57:30:@34.8]
    node _T_30 = asUInt(_T_29) @[AdvTesterSpec.scala 57:30:@35.8]
    node _T_31 = tail(_T_30, 1) @[AdvTesterSpec.scala 57:30:@36.8]
    node _GEN_3 = mux(_T_28, y, _GEN_0) @[AdvTesterSpec.scala 56:19:@29.6]
    node _GEN_4 = mux(_T_28, x, _T_31) @[AdvTesterSpec.scala 56:19:@29.6]
    node _T_32 = bits(reset, 0, 0) @[AdvTesterSpec.scala 58:11:@39.6]
    node _T_34 = eq(_T_32, UInt<1>("h0")) @[AdvTesterSpec.scala 58:11:@40.6]
    node _T_35 = bits(reset, 0, 0) @[AdvTesterSpec.scala 59:23:@46.6]
    node _T_37 = eq(_T_35, UInt<1>("h0")) @[AdvTesterSpec.scala 59:23:@47.6]
    node _GEN_5 = mux(p, _GEN_3, _GEN_0) @[AdvTesterSpec.scala 55:12:@27.4]
    node _GEN_6 = mux(p, _GEN_4, _GEN_1) @[AdvTesterSpec.scala 55:12:@27.4]
    node _T_39 = eq(y, UInt<1>("h0")) @[AdvTesterSpec.scala 62:21:@53.4]
    node _T_40 = and(_T_39, p) @[AdvTesterSpec.scala 62:29:@54.4]
    node _GEN_7 = mux(io_out_valid, UInt<1>("h0"), _GEN_2) @[AdvTesterSpec.scala 63:23:@56.4]
    io_in_ready <= _T_23
    io_out_valid <= _T_40
    io_out_bits <= x
    x <= _GEN_5
    y <= _GEN_6
    p <= mux(reset, UInt<1>("h0"), _GEN_7)
    ti <= mux(reset, UInt<16>("h0"), _T_21)
    printf(clock, and(and(and(UInt<1>("h1"), p), _T_34), UInt<1>("h1")), "x: %d, y: %d\n", x, y) @[AdvTesterSpec.scala 58:11:@42.8]
    printf(clock, and(and(and(UInt<1>("h1"), eq(p, UInt<1>("h0"))), _T_37), UInt<1>("h1")), "stalled\n") @[AdvTesterSpec.scala 59:23:@49.8]
