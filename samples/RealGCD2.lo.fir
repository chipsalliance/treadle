circuit RealGCD2 : @[:@2.0]
  module RealGCD2 : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    output io_in_ready : UInt<1> @[:@6.4]
    input io_in_valid : UInt<1> @[:@6.4]
    input io_in_bits_a : UInt<16> @[:@6.4]
    input io_in_bits_b : UInt<16> @[:@6.4]
    output io_out_valid : UInt<1> @[:@6.4]
    output io_out_bits : UInt<16> @[:@6.4]
  
    reg x : UInt<16>, clock with :
      reset => (UInt<1>("h0"), x) @[GCDSpec.scala 38:14:@11.4]
    reg y : UInt<16>, clock with :
      reset => (UInt<1>("h0"), y) @[GCDSpec.scala 39:14:@12.4]
    reg p : UInt<1>, clock with :
      reset => (UInt<1>("h0"), p) @[GCDSpec.scala 40:18:@13.4]
    reg ti : UInt<16>, clock with :
      reset => (UInt<1>("h0"), ti) @[GCDSpec.scala 42:19:@14.4]
    node _T_20 = add(ti, UInt<1>("h1")) @[GCDSpec.scala 43:12:@15.4]
    node _T_21 = tail(_T_20, 1) @[GCDSpec.scala 43:12:@16.4]
    node _T_23 = eq(p, UInt<1>("h0")) @[GCDSpec.scala 45:18:@18.4]
    node _T_25 = eq(p, UInt<1>("h0")) @[GCDSpec.scala 47:24:@20.4]
    node _T_26 = and(io_in_valid, _T_25) @[GCDSpec.scala 47:21:@21.4]
    node _GEN_0 = mux(_T_26, io_in_bits_a, x) @[GCDSpec.scala 47:28:@22.4]
    node _GEN_1 = mux(_T_26, io_in_bits_b, y) @[GCDSpec.scala 47:28:@22.4]
    node _GEN_2 = mux(_T_26, UInt<1>("h1"), p) @[GCDSpec.scala 47:28:@22.4]
    node _T_28 = gt(x, y) @[GCDSpec.scala 54:13:@28.6]
    node _T_29 = sub(y, x) @[GCDSpec.scala 55:30:@34.8]
    node _T_30 = asUInt(_T_29) @[GCDSpec.scala 55:30:@35.8]
    node _T_31 = tail(_T_30, 1) @[GCDSpec.scala 55:30:@36.8]
    node _GEN_3 = mux(_T_28, y, _GEN_0) @[GCDSpec.scala 54:19:@29.6]
    node _GEN_4 = mux(_T_28, x, _T_31) @[GCDSpec.scala 54:19:@29.6]
    node _GEN_5 = mux(p, _GEN_3, _GEN_0) @[GCDSpec.scala 53:12:@27.4]
    node _GEN_6 = mux(p, _GEN_4, _GEN_1) @[GCDSpec.scala 53:12:@27.4]
    node _T_33 = eq(y, UInt<1>("h0")) @[GCDSpec.scala 59:21:@41.4]
    node _T_34 = and(_T_33, p) @[GCDSpec.scala 59:29:@42.4]
    node _GEN_7 = mux(io_out_valid, UInt<1>("h0"), _GEN_2) @[GCDSpec.scala 60:23:@44.4]
    io_in_ready <= _T_23
    io_out_valid <= _T_34
    io_out_bits <= x
    x <= _GEN_5
    y <= _GEN_6
    p <= mux(reset, UInt<1>("h0"), _GEN_7)
    ti <= mux(reset, UInt<16>("h0"), _T_21)
