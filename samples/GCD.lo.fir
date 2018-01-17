circuit GCD : @[:@2.0]
  module GCD : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<16> @[:@6.4]
    input io_b : UInt<16> @[:@6.4]
    input io_e : UInt<1> @[:@6.4]
    output io_z : UInt<16> @[:@6.4]
    output io_v : UInt<1> @[:@6.4]
  
    reg x : UInt<16>, clock with :
      reset => (UInt<1>("h0"), x) @[GCD.scala 17:15:@11.4]
    reg y : UInt<16>, clock with :
      reset => (UInt<1>("h0"), y) @[GCD.scala 18:15:@12.4]
    node _T_9 = gt(x, y) @[GCD.scala 20:11:@13.4]
    node _T_10 = sub(x, y) @[GCD.scala 20:25:@15.6]
    node _T_11 = asUInt(_T_10) @[GCD.scala 20:25:@16.6]
    node _T_12 = tail(_T_11, 1) @[GCD.scala 20:25:@17.6]
    node _GEN_0 = mux(_T_9, _T_12, x) @[GCD.scala 20:16:@14.4]
    node _T_14 = eq(_T_9, UInt<1>("h0")) @[GCD.scala 20:16:@20.4]
    node _T_15 = sub(y, x) @[GCD.scala 21:25:@22.6]
    node _T_16 = asUInt(_T_15) @[GCD.scala 21:25:@23.6]
    node _T_17 = tail(_T_16, 1) @[GCD.scala 21:25:@24.6]
    node _GEN_1 = mux(_T_14, _T_17, y) @[GCD.scala 21:16:@21.4]
    node _GEN_2 = mux(io_e, io_a, _GEN_0) @[GCD.scala 23:15:@27.4]
    node _GEN_3 = mux(io_e, io_b, _GEN_1) @[GCD.scala 23:15:@27.4]
    node _T_19 = eq(y, UInt<1>("h0")) @[GCD.scala 25:13:@32.4]
    io_z <= x
    io_v <= _T_19
    x <= _GEN_2
    y <= _GEN_3
