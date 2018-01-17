circuit MultiGcdCalculator : @[:@2.0]
  module GcdEngine : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    output io_decoupledInput_ready : UInt<1> @[:@6.4]
    input io_decoupledInput_valid : UInt<1> @[:@6.4]
    input io_decoupledInput_bits_a : UInt<16> @[:@6.4]
    input io_decoupledInput_bits_b : UInt<16> @[:@6.4]
    output io_validOutput_valid : UInt<1> @[:@6.4]
    output io_validOutput_bits_a : UInt<16> @[:@6.4]
    output io_validOutput_bits_b : UInt<16> @[:@6.4]
    output io_validOutput_bits_gcd : UInt<16> @[:@6.4]
  
    reg x : UInt<16>, clock with :
      reset => (UInt<1>("h0"), x) @[ConcurrentDecoupledTestingSpec.scala 38:14:@11.4]
    reg y : UInt<16>, clock with :
      reset => (UInt<1>("h0"), y) @[ConcurrentDecoupledTestingSpec.scala 39:14:@12.4]
    reg xOriginal : UInt<16>, clock with :
      reset => (UInt<1>("h0"), xOriginal) @[ConcurrentDecoupledTestingSpec.scala 40:22:@13.4]
    reg yOriginal : UInt<16>, clock with :
      reset => (UInt<1>("h0"), yOriginal) @[ConcurrentDecoupledTestingSpec.scala 41:22:@14.4]
    reg busy : UInt<1>, clock with :
      reset => (UInt<1>("h0"), busy) @[ConcurrentDecoupledTestingSpec.scala 42:21:@15.4]
    node _T_20 = eq(busy, UInt<1>("h0")) @[ConcurrentDecoupledTestingSpec.scala 44:30:@16.4]
    node _T_22 = eq(busy, UInt<1>("h0")) @[ConcurrentDecoupledTestingSpec.scala 46:35:@18.4]
    node _T_23 = and(io_decoupledInput_valid, _T_22) @[ConcurrentDecoupledTestingSpec.scala 46:32:@19.4]
    node _GEN_0 = mux(_T_23, io_decoupledInput_bits_a, x) @[ConcurrentDecoupledTestingSpec.scala 46:42:@20.4]
    node _GEN_1 = mux(_T_23, io_decoupledInput_bits_b, y) @[ConcurrentDecoupledTestingSpec.scala 46:42:@20.4]
    node _GEN_2 = mux(_T_23, io_decoupledInput_bits_a, xOriginal) @[ConcurrentDecoupledTestingSpec.scala 46:42:@20.4]
    node _GEN_3 = mux(_T_23, io_decoupledInput_bits_b, yOriginal) @[ConcurrentDecoupledTestingSpec.scala 46:42:@20.4]
    node _GEN_4 = mux(_T_23, UInt<1>("h1"), busy) @[ConcurrentDecoupledTestingSpec.scala 46:42:@20.4]
    node _T_25 = gt(x, y) @[ConcurrentDecoupledTestingSpec.scala 55:12:@28.6]
    node _T_26 = sub(y, x) @[ConcurrentDecoupledTestingSpec.scala 60:16:@34.8]
    node _T_27 = asUInt(_T_26) @[ConcurrentDecoupledTestingSpec.scala 60:16:@35.8]
    node _T_28 = tail(_T_27, 1) @[ConcurrentDecoupledTestingSpec.scala 60:16:@36.8]
    node _GEN_5 = mux(_T_25, y, _GEN_0) @[ConcurrentDecoupledTestingSpec.scala 55:17:@29.6]
    node _GEN_6 = mux(_T_25, x, _T_28) @[ConcurrentDecoupledTestingSpec.scala 55:17:@29.6]
    node _GEN_7 = mux(busy, _GEN_5, _GEN_0) @[ConcurrentDecoupledTestingSpec.scala 54:14:@27.4]
    node _GEN_8 = mux(busy, _GEN_6, _GEN_1) @[ConcurrentDecoupledTestingSpec.scala 54:14:@27.4]
    node _T_30 = eq(y, UInt<1>("h0")) @[ConcurrentDecoupledTestingSpec.scala 67:32:@43.4]
    node _T_31 = and(_T_30, busy) @[ConcurrentDecoupledTestingSpec.scala 67:40:@44.4]
    node _GEN_9 = mux(io_validOutput_valid, UInt<1>("h0"), _GEN_4) @[ConcurrentDecoupledTestingSpec.scala 68:30:@46.4]
    io_decoupledInput_ready <= _T_20
    io_validOutput_valid <= _T_31
    io_validOutput_bits_a <= xOriginal
    io_validOutput_bits_b <= yOriginal
    io_validOutput_bits_gcd <= x
    x <= _GEN_7
    y <= _GEN_8
    xOriginal <= _GEN_2
    yOriginal <= _GEN_3
    busy <= mux(reset, UInt<1>("h0"), _GEN_9)

  module MultiGcdCalculator : @[:@191.2]
    input clock : Clock @[:@192.4]
    input reset : UInt<1> @[:@193.4]
    output io_input_0_ready : UInt<1> @[:@194.4]
    input io_input_0_valid : UInt<1> @[:@194.4]
    input io_input_0_bits_a : UInt<16> @[:@194.4]
    input io_input_0_bits_b : UInt<16> @[:@194.4]
    output io_input_1_ready : UInt<1> @[:@194.4]
    input io_input_1_valid : UInt<1> @[:@194.4]
    input io_input_1_bits_a : UInt<16> @[:@194.4]
    input io_input_1_bits_b : UInt<16> @[:@194.4]
    output io_input_2_ready : UInt<1> @[:@194.4]
    input io_input_2_valid : UInt<1> @[:@194.4]
    input io_input_2_bits_a : UInt<16> @[:@194.4]
    input io_input_2_bits_b : UInt<16> @[:@194.4]
    output io_input_3_ready : UInt<1> @[:@194.4]
    input io_input_3_valid : UInt<1> @[:@194.4]
    input io_input_3_bits_a : UInt<16> @[:@194.4]
    input io_input_3_bits_b : UInt<16> @[:@194.4]
    output io_output_0_valid : UInt<1> @[:@194.4]
    output io_output_0_bits_a : UInt<16> @[:@194.4]
    output io_output_0_bits_b : UInt<16> @[:@194.4]
    output io_output_0_bits_gcd : UInt<16> @[:@194.4]
    output io_output_1_valid : UInt<1> @[:@194.4]
    output io_output_1_bits_a : UInt<16> @[:@194.4]
    output io_output_1_bits_b : UInt<16> @[:@194.4]
    output io_output_1_bits_gcd : UInt<16> @[:@194.4]
    output io_output_2_valid : UInt<1> @[:@194.4]
    output io_output_2_bits_a : UInt<16> @[:@194.4]
    output io_output_2_bits_b : UInt<16> @[:@194.4]
    output io_output_2_bits_gcd : UInt<16> @[:@194.4]
    output io_output_3_valid : UInt<1> @[:@194.4]
    output io_output_3_bits_a : UInt<16> @[:@194.4]
    output io_output_3_bits_b : UInt<16> @[:@194.4]
    output io_output_3_bits_gcd : UInt<16> @[:@194.4]
  
    inst GcdEngine of GcdEngine @[ConcurrentDecoupledTestingSpec.scala 84:24:@199.4]
    inst GcdEngine_1 of GcdEngine @[ConcurrentDecoupledTestingSpec.scala 84:24:@211.4]
    inst GcdEngine_2 of GcdEngine @[ConcurrentDecoupledTestingSpec.scala 84:24:@223.4]
    inst GcdEngine_3 of GcdEngine @[ConcurrentDecoupledTestingSpec.scala 84:24:@235.4]
    io_input_0_ready <= GcdEngine.io_decoupledInput_ready
    io_input_1_ready <= GcdEngine_1.io_decoupledInput_ready
    io_input_2_ready <= GcdEngine_2.io_decoupledInput_ready
    io_input_3_ready <= GcdEngine_3.io_decoupledInput_ready
    io_output_0_valid <= GcdEngine.io_validOutput_valid
    io_output_0_bits_a <= GcdEngine.io_validOutput_bits_a
    io_output_0_bits_b <= GcdEngine.io_validOutput_bits_b
    io_output_0_bits_gcd <= GcdEngine.io_validOutput_bits_gcd
    io_output_1_valid <= GcdEngine_1.io_validOutput_valid
    io_output_1_bits_a <= GcdEngine_1.io_validOutput_bits_a
    io_output_1_bits_b <= GcdEngine_1.io_validOutput_bits_b
    io_output_1_bits_gcd <= GcdEngine_1.io_validOutput_bits_gcd
    io_output_2_valid <= GcdEngine_2.io_validOutput_valid
    io_output_2_bits_a <= GcdEngine_2.io_validOutput_bits_a
    io_output_2_bits_b <= GcdEngine_2.io_validOutput_bits_b
    io_output_2_bits_gcd <= GcdEngine_2.io_validOutput_bits_gcd
    io_output_3_valid <= GcdEngine_3.io_validOutput_valid
    io_output_3_bits_a <= GcdEngine_3.io_validOutput_bits_a
    io_output_3_bits_b <= GcdEngine_3.io_validOutput_bits_b
    io_output_3_bits_gcd <= GcdEngine_3.io_validOutput_bits_gcd
    GcdEngine.io_decoupledInput_valid <= io_input_0_valid
    GcdEngine.io_decoupledInput_bits_a <= io_input_0_bits_a
    GcdEngine.io_decoupledInput_bits_b <= io_input_0_bits_b
    GcdEngine.clock <= clock
    GcdEngine.reset <= reset
    GcdEngine_1.io_decoupledInput_valid <= io_input_1_valid
    GcdEngine_1.io_decoupledInput_bits_a <= io_input_1_bits_a
    GcdEngine_1.io_decoupledInput_bits_b <= io_input_1_bits_b
    GcdEngine_1.clock <= clock
    GcdEngine_1.reset <= reset
    GcdEngine_2.io_decoupledInput_valid <= io_input_2_valid
    GcdEngine_2.io_decoupledInput_bits_a <= io_input_2_bits_a
    GcdEngine_2.io_decoupledInput_bits_b <= io_input_2_bits_b
    GcdEngine_2.clock <= clock
    GcdEngine_2.reset <= reset
    GcdEngine_3.io_decoupledInput_valid <= io_input_3_valid
    GcdEngine_3.io_decoupledInput_bits_a <= io_input_3_bits_a
    GcdEngine_3.io_decoupledInput_bits_b <= io_input_3_bits_b
    GcdEngine_3.clock <= clock
    GcdEngine_3.reset <= reset
