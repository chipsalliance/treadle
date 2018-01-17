circuit ExampleModule : @[:@2.0]
  module ExampleModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    output io_in_ready : UInt<1> @[:@6.4]
    input io_in_valid : UInt<1> @[:@6.4]
    input io_in_bits : UInt<16> @[:@6.4]
    input io_out_ready : UInt<1> @[:@6.4]
    output io_out_valid : UInt<1> @[:@6.4]
    output io_out_bits : UInt<16> @[:@6.4]
  
    reg busy : UInt<1>, clock with :
      reset => (UInt<1>("h0"), busy) @[InterpreterVerilatorConsistencySpec.scala 20:21:@11.4]
    reg in_reg : UInt<16>, clock with :
      reset => (UInt<1>("h0"), in_reg) @[InterpreterVerilatorConsistencySpec.scala 21:25:@12.4]
    node _T_19 = eq(busy, UInt<1>("h0")) @[InterpreterVerilatorConsistencySpec.scala 22:18:@13.4]
    node _T_21 = eq(busy, UInt<1>("h0")) @[InterpreterVerilatorConsistencySpec.scala 24:23:@15.4]
    node _T_22 = and(io_in_valid, _T_21) @[InterpreterVerilatorConsistencySpec.scala 24:20:@16.4]
    node _GEN_0 = mux(_T_22, io_in_bits, in_reg) @[InterpreterVerilatorConsistencySpec.scala 24:30:@17.4]
    node _GEN_1 = mux(_T_22, UInt<1>("h1"), busy) @[InterpreterVerilatorConsistencySpec.scala 24:30:@17.4]
    reg wait_counter : UInt<16>, clock with :
      reset => (UInt<1>("h0"), wait_counter) @[InterpreterVerilatorConsistencySpec.scala 29:29:@21.4]
    node _T_27 = eq(busy, UInt<1>("h0")) @[InterpreterVerilatorConsistencySpec.scala 31:23:@22.4]
    node _T_28 = and(io_in_valid, _T_27) @[InterpreterVerilatorConsistencySpec.scala 31:20:@23.4]
    node _GEN_2 = mux(_T_28, UInt<1>("h0"), wait_counter) @[InterpreterVerilatorConsistencySpec.scala 31:30:@24.4]
    node _T_30 = eq(wait_counter, UInt<3>("h5")) @[InterpreterVerilatorConsistencySpec.scala 36:23:@28.6]
    node _T_33 = add(wait_counter, UInt<1>("h1")) @[InterpreterVerilatorConsistencySpec.scala 40:36:@34.8]
    node _T_34 = tail(_T_33, 1) @[InterpreterVerilatorConsistencySpec.scala 40:36:@35.8]
    node _GEN_3 = mux(_T_30, in_reg, UInt<1>("h0")) @[InterpreterVerilatorConsistencySpec.scala 36:40:@29.6]
    node _GEN_4 = mux(_T_30, _GEN_2, _T_34) @[InterpreterVerilatorConsistencySpec.scala 36:40:@29.6]
    node _GEN_5 = validif(busy, _GEN_3) @[InterpreterVerilatorConsistencySpec.scala 35:14:@27.4]
    node _GEN_6 = mux(busy, _GEN_4, _GEN_2) @[InterpreterVerilatorConsistencySpec.scala 35:14:@27.4]
    node _T_35 = eq(io_out_bits, in_reg) @[InterpreterVerilatorConsistencySpec.scala 44:32:@39.4]
    node _T_36 = eq(wait_counter, UInt<3>("h5")) @[InterpreterVerilatorConsistencySpec.scala 44:61:@40.4]
    node _T_37 = and(_T_35, _T_36) @[InterpreterVerilatorConsistencySpec.scala 44:44:@41.4]
    node _T_38 = and(_T_37, busy) @[InterpreterVerilatorConsistencySpec.scala 44:78:@42.4]
    node _T_39 = bits(reset, 0, 0) @[InterpreterVerilatorConsistencySpec.scala 46:9:@44.4]
    node _T_41 = eq(_T_39, UInt<1>("h0")) @[InterpreterVerilatorConsistencySpec.scala 46:9:@45.4]
    io_in_ready <= _T_19
    io_out_valid <= _T_38
    io_out_bits <= _GEN_5
    busy <= mux(reset, UInt<1>("h0"), _GEN_1)
    in_reg <= mux(reset, UInt<16>("h0"), _GEN_0)
    wait_counter <= mux(reset, UInt<16>("h0"), _GEN_6)
    printf(clock, and(and(UInt<1>("h1"), _T_41), UInt<1>("h1")), "From printf -- in: ready %d   valid %d   value %d  -- out:  ready %d  valid %d  value %d\n", io_in_ready, io_in_valid, io_in_bits, io_out_ready, io_out_valid, io_out_bits) @[InterpreterVerilatorConsistencySpec.scala 46:9:@47.6]
