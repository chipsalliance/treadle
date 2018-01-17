circuit ConstantTapTransposedStreamingFIR : @[:@2.0]
  module ConstantTapTransposedStreamingFIR : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_input_valid : UInt<1> @[:@6.4]
    input io_input_bits : SInt<10> @[:@6.4]
    output io_output_valid : UInt<1> @[:@6.4]
    output io_output_bits : SInt<16> @[:@6.4]
  
    node products_0 = mul(io_input_bits, asSInt(UInt<3>("h2"))) @[SIntTypeClass.scala 44:41:@11.4]
    node products_1 = mul(io_input_bits, asSInt(UInt<2>("h1"))) @[SIntTypeClass.scala 44:41:@12.4]
    node products_2 = mul(io_input_bits, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 44:41:@13.4]
    reg last : SInt<16>, clock with :
      reset => (UInt<1>("h0"), last) @[TransposedStreamingFIR.scala 33:20:@14.4]
    reg _T_16 : SInt<13>, clock with :
      reset => (UInt<1>("h0"), _T_16) @[TransposedStreamingFIR.scala 35:18:@15.4]
    node _GEN_0 = mux(io_input_valid, products_0, _T_16) @[TransposedStreamingFIR.scala 36:27:@16.4]
    node _T_17 = add(_T_16, products_1) @[SIntTypeClass.scala 18:40:@19.4]
    node _T_18 = tail(_T_17, 1) @[SIntTypeClass.scala 18:40:@20.4]
    node _T_19 = asSInt(_T_18) @[SIntTypeClass.scala 18:40:@21.4]
    reg _T_21 : SInt<13>, clock with :
      reset => (UInt<1>("h0"), _T_21) @[TransposedStreamingFIR.scala 35:18:@22.4]
    node _GEN_1 = mux(io_input_valid, _T_19, _T_21) @[TransposedStreamingFIR.scala 36:27:@23.4]
    node _T_22 = add(_T_21, products_2) @[SIntTypeClass.scala 18:40:@26.4]
    node _T_23 = tail(_T_22, 1) @[SIntTypeClass.scala 18:40:@27.4]
    node nextLast = asSInt(_T_23) @[SIntTypeClass.scala 18:40:@28.4]
    node _GEN_2 = mux(io_input_valid, nextLast, last) @[TransposedStreamingFIR.scala 41:24:@29.4]
    reg _T_25 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), _T_25) @[TransposedStreamingFIR.scala 46:29:@33.4]
    io_output_valid <= _T_25
    io_output_bits <= last
    last <= _GEN_2
    _T_16 <= _GEN_0
    _T_21 <= _GEN_1
    _T_25 <= io_input_valid
