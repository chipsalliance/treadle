circuit Sort : @[:@2.0]
  module Sort : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_inputs_0 : SInt<16> @[:@6.4]
    input io_inputs_1 : SInt<16> @[:@6.4]
    input io_inputs_2 : SInt<16> @[:@6.4]
    input io_inputs_3 : SInt<16> @[:@6.4]
    input io_inputs_4 : SInt<16> @[:@6.4]
    input io_newInputs : UInt<1> @[:@6.4]
    output io_outputs_0 : SInt<16> @[:@6.4]
    output io_outputs_1 : SInt<16> @[:@6.4]
    output io_outputs_2 : SInt<16> @[:@6.4]
    output io_outputs_3 : SInt<16> @[:@6.4]
    output io_outputs_4 : SInt<16> @[:@6.4]
    output io_sortDone : UInt<1> @[:@6.4]
  
    reg orderedRegs_0 : SInt<64>, clock with :
      reset => (UInt<1>("h0"), orderedRegs_0) @[Sort.scala 21:25:@11.4]
    reg orderedRegs_1 : SInt<64>, clock with :
      reset => (UInt<1>("h0"), orderedRegs_1) @[Sort.scala 21:25:@11.4]
    reg orderedRegs_2 : SInt<64>, clock with :
      reset => (UInt<1>("h0"), orderedRegs_2) @[Sort.scala 21:25:@11.4]
    reg orderedRegs_3 : SInt<64>, clock with :
      reset => (UInt<1>("h0"), orderedRegs_3) @[Sort.scala 21:25:@11.4]
    reg orderedRegs_4 : SInt<64>, clock with :
      reset => (UInt<1>("h0"), orderedRegs_4) @[Sort.scala 21:25:@11.4]
    reg busy : UInt<1>, clock with :
      reset => (UInt<1>("h0"), busy) @[Sort.scala 22:29:@12.4]
    reg sortCounter : UInt<3>, clock with :
      reset => (UInt<1>("h0"), sortCounter) @[Sort.scala 23:29:@13.4]
    reg isEvenCycle : UInt<1>, clock with :
      reset => (UInt<1>("h0"), isEvenCycle) @[Sort.scala 24:29:@14.4]
    node _T_42 = eq(isEvenCycle, UInt<1>("h0")) @[Sort.scala 35:20:@27.8]
    node _T_44 = add(sortCounter, UInt<1>("h1")) @[Sort.scala 37:32:@29.8]
    node _T_45 = tail(_T_44, 1) @[Sort.scala 37:32:@30.8]
    node _T_47 = gt(sortCounter, UInt<3>("h5")) @[Sort.scala 38:22:@32.8]
    node _GEN_0 = mux(_T_47, UInt<1>("h0"), busy) @[Sort.scala 38:37:@33.8]
    node _T_49 = gt(orderedRegs_0, orderedRegs_1) @[Sort.scala 45:21:@37.10]
    node _GEN_1 = mux(_T_49, orderedRegs_1, orderedRegs_0) @[Sort.scala 45:29:@38.10]
    node _GEN_2 = mux(_T_49, orderedRegs_0, orderedRegs_1) @[Sort.scala 45:29:@38.10]
    node _T_50 = gt(orderedRegs_2, orderedRegs_3) @[Sort.scala 45:21:@42.10]
    node _GEN_3 = mux(_T_50, orderedRegs_3, orderedRegs_2) @[Sort.scala 45:29:@43.10]
    node _GEN_4 = mux(_T_50, orderedRegs_2, orderedRegs_3) @[Sort.scala 45:29:@43.10]
    node _T_51 = gt(orderedRegs_1, orderedRegs_2) @[Sort.scala 55:21:@49.10]
    node _GEN_5 = mux(_T_51, orderedRegs_2, orderedRegs_1) @[Sort.scala 55:29:@50.10]
    node _GEN_6 = mux(_T_51, orderedRegs_1, orderedRegs_2) @[Sort.scala 55:29:@50.10]
    node _T_52 = gt(orderedRegs_3, orderedRegs_4) @[Sort.scala 55:21:@54.10]
    node _GEN_7 = mux(_T_52, orderedRegs_4, orderedRegs_3) @[Sort.scala 55:29:@55.10]
    node _GEN_8 = mux(_T_52, orderedRegs_3, orderedRegs_4) @[Sort.scala 55:29:@55.10]
    node _GEN_9 = mux(isEvenCycle, _GEN_1, orderedRegs_0) @[Sort.scala 42:23:@36.8]
    node _GEN_10 = mux(isEvenCycle, _GEN_2, _GEN_5) @[Sort.scala 42:23:@36.8]
    node _GEN_11 = mux(isEvenCycle, _GEN_3, _GEN_6) @[Sort.scala 42:23:@36.8]
    node _GEN_12 = mux(isEvenCycle, _GEN_4, _GEN_7) @[Sort.scala 42:23:@36.8]
    node _GEN_13 = mux(isEvenCycle, orderedRegs_4, _GEN_8) @[Sort.scala 42:23:@36.8]
    node _GEN_14 = mux(busy, _T_42, isEvenCycle) @[Sort.scala 34:19:@26.6]
    node _GEN_15 = mux(busy, _T_45, sortCounter) @[Sort.scala 34:19:@26.6]
    node _GEN_16 = mux(busy, _GEN_0, busy) @[Sort.scala 34:19:@26.6]
    node _GEN_17 = mux(busy, _GEN_9, orderedRegs_0) @[Sort.scala 34:19:@26.6]
    node _GEN_18 = mux(busy, _GEN_10, orderedRegs_1) @[Sort.scala 34:19:@26.6]
    node _GEN_19 = mux(busy, _GEN_11, orderedRegs_2) @[Sort.scala 34:19:@26.6]
    node _GEN_20 = mux(busy, _GEN_12, orderedRegs_3) @[Sort.scala 34:19:@26.6]
    node _GEN_21 = mux(busy, _GEN_13, orderedRegs_4) @[Sort.scala 34:19:@26.6]
    node _GEN_22 = mux(io_newInputs, shl(io_inputs_0, 8), _GEN_17) @[Sort.scala 26:22:@15.4]
    node _GEN_23 = mux(io_newInputs, shl(io_inputs_1, 8), _GEN_18) @[Sort.scala 26:22:@15.4]
    node _GEN_24 = mux(io_newInputs, shl(io_inputs_2, 8), _GEN_19) @[Sort.scala 26:22:@15.4]
    node _GEN_25 = mux(io_newInputs, shl(io_inputs_3, 8), _GEN_20) @[Sort.scala 26:22:@15.4]
    node _GEN_26 = mux(io_newInputs, shl(io_inputs_4, 8), _GEN_21) @[Sort.scala 26:22:@15.4]
    node _GEN_27 = mux(io_newInputs, UInt<1>("h1"), _GEN_16) @[Sort.scala 26:22:@15.4]
    node _GEN_28 = mux(io_newInputs, UInt<1>("h0"), _GEN_15) @[Sort.scala 26:22:@15.4]
    node _GEN_29 = mux(io_newInputs, UInt<1>("h0"), _GEN_14) @[Sort.scala 26:22:@15.4]
    node _T_54 = eq(busy, UInt<1>("h0")) @[Sort.scala 64:18:@61.4]
    io_outputs_0 <= asSInt(bits(shr(orderedRegs_0, 8), 15, 0))
    io_outputs_1 <= asSInt(bits(shr(orderedRegs_1, 8), 15, 0))
    io_outputs_2 <= asSInt(bits(shr(orderedRegs_2, 8), 15, 0))
    io_outputs_3 <= asSInt(bits(shr(orderedRegs_3, 8), 15, 0))
    io_outputs_4 <= asSInt(bits(shr(orderedRegs_4, 8), 15, 0))
    io_sortDone <= _T_54
    orderedRegs_0 <= _GEN_22
    orderedRegs_1 <= _GEN_23
    orderedRegs_2 <= _GEN_24
    orderedRegs_3 <= _GEN_25
    orderedRegs_4 <= _GEN_26
    busy <= mux(reset, UInt<1>("h0"), _GEN_27)
    sortCounter <= mux(reset, UInt<3>("h0"), _GEN_28)
    isEvenCycle <= mux(reset, UInt<1>("h0"), _GEN_29)
