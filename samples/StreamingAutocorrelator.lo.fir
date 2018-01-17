circuit StreamingAutocorrelator : @[:@2.0]
  module TransposedStreamingFIR : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_input : SInt<10> @[:@6.4]
    output io_output : SInt<20> @[:@6.4]
    input io_taps_0 : SInt<10> @[:@6.4]
    input io_taps_1 : SInt<10> @[:@6.4]
    input io_taps_2 : SInt<10> @[:@6.4]
  
    node products_0 = mul(io_input, io_taps_2) @[SIntTypeClass.scala 44:41:@11.4]
    node products_1 = mul(io_input, io_taps_1) @[SIntTypeClass.scala 44:41:@12.4]
    node products_2 = mul(io_input, io_taps_0) @[SIntTypeClass.scala 44:41:@13.4]
    reg last : SInt<20>, clock with :
      reset => (UInt<1>("h0"), last) @[TransposedStreamingFIR.scala 62:17:@14.4]
    reg _T_14 : SInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_14) @[TransposedStreamingFIR.scala 64:18:@15.4]
    node _T_15 = add(_T_14, products_1) @[SIntTypeClass.scala 18:40:@17.4]
    node _T_16 = tail(_T_15, 1) @[SIntTypeClass.scala 18:40:@18.4]
    node _T_17 = asSInt(_T_16) @[SIntTypeClass.scala 18:40:@19.4]
    reg _T_19 : SInt<20>, clock with :
      reset => (UInt<1>("h0"), _T_19) @[TransposedStreamingFIR.scala 64:18:@20.4]
    node _T_20 = add(_T_19, products_2) @[SIntTypeClass.scala 18:40:@22.4]
    node _T_21 = tail(_T_20, 1) @[SIntTypeClass.scala 18:40:@23.4]
    node _T_22 = asSInt(_T_21) @[SIntTypeClass.scala 18:40:@24.4]
    io_output <= last
    last <= _T_22
    _T_14 <= products_0
    _T_19 <= _T_17

  module StreamingAutocorrelator : @[:@28.2]
    input clock : Clock @[:@29.4]
    input reset : UInt<1> @[:@30.4]
    input io_input : SInt<10> @[:@31.4]
    output io_output : SInt<20> @[:@31.4]
  
    reg delays_1 : SInt<10>, clock with :
      reset => (UInt<1>("h0"), delays_1) @[StreamingAutocorrelator.scala 20:22:@36.4]
    reg delays_2 : SInt<10>, clock with :
      reset => (UInt<1>("h0"), delays_2) @[StreamingAutocorrelator.scala 20:22:@38.4]
    reg delays_3 : SInt<10>, clock with :
      reset => (UInt<1>("h0"), delays_3) @[StreamingAutocorrelator.scala 20:22:@40.4]
    reg delays_4 : SInt<10>, clock with :
      reset => (UInt<1>("h0"), delays_4) @[StreamingAutocorrelator.scala 20:22:@42.4]
    reg delays_5 : SInt<10>, clock with :
      reset => (UInt<1>("h0"), delays_5) @[StreamingAutocorrelator.scala 20:22:@44.4]
    inst firFilter of TransposedStreamingFIR @[StreamingAutocorrelator.scala 28:11:@46.4]
    io_output <= firFilter.io_output
    delays_1 <= io_input
    delays_2 <= delays_1
    delays_3 <= delays_2
    delays_4 <= delays_3
    delays_5 <= delays_4
    firFilter.io_input <= io_input
    firFilter.io_taps_0 <= delays_5
    firFilter.io_taps_1 <= delays_4
    firFilter.io_taps_2 <= delays_3
    firFilter.clock <= clock
    firFilter.reset <= reset
