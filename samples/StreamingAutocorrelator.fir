;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit StreamingAutocorrelator : 
  module TransposedStreamingFIR : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input : SInt<10>, output : SInt<20>, flip taps : SInt<10>[3]}
    
    clock is invalid
    reset is invalid
    io is invalid
    node products_0 = mul(io.input, io.taps[2]) @[SIntTypeClass.scala 44:41]
    node products_1 = mul(io.input, io.taps[1]) @[SIntTypeClass.scala 44:41]
    node products_2 = mul(io.input, io.taps[0]) @[SIntTypeClass.scala 44:41]
    reg last : SInt<20>, clock @[TransposedStreamingFIR.scala 62:17]
    reg _T_14 : SInt<20>, clock @[TransposedStreamingFIR.scala 64:18]
    _T_14 <= products_0 @[TransposedStreamingFIR.scala 65:9]
    node _T_15 = add(_T_14, products_1) @[SIntTypeClass.scala 18:40]
    node _T_16 = tail(_T_15, 1) @[SIntTypeClass.scala 18:40]
    node _T_17 = asSInt(_T_16) @[SIntTypeClass.scala 18:40]
    reg _T_19 : SInt<20>, clock @[TransposedStreamingFIR.scala 64:18]
    _T_19 <= _T_17 @[TransposedStreamingFIR.scala 65:9]
    node _T_20 = add(_T_19, products_2) @[SIntTypeClass.scala 18:40]
    node _T_21 = tail(_T_20, 1) @[SIntTypeClass.scala 18:40]
    node _T_22 = asSInt(_T_21) @[SIntTypeClass.scala 18:40]
    last <= _T_22 @[TransposedStreamingFIR.scala 63:8]
    io.output <= last @[TransposedStreamingFIR.scala 69:13]
    
  module StreamingAutocorrelator : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input : SInt<10>, output : SInt<20>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg delays_1 : SInt<10>, clock @[StreamingAutocorrelator.scala 20:22]
    delays_1 <= io.input @[StreamingAutocorrelator.scala 21:13]
    reg delays_2 : SInt<10>, clock @[StreamingAutocorrelator.scala 20:22]
    delays_2 <= delays_1 @[StreamingAutocorrelator.scala 21:13]
    reg delays_3 : SInt<10>, clock @[StreamingAutocorrelator.scala 20:22]
    delays_3 <= delays_2 @[StreamingAutocorrelator.scala 21:13]
    reg delays_4 : SInt<10>, clock @[StreamingAutocorrelator.scala 20:22]
    delays_4 <= delays_3 @[StreamingAutocorrelator.scala 21:13]
    reg delays_5 : SInt<10>, clock @[StreamingAutocorrelator.scala 20:22]
    delays_5 <= delays_4 @[StreamingAutocorrelator.scala 21:13]
    inst firFilter of TransposedStreamingFIR @[StreamingAutocorrelator.scala 28:11]
    firFilter.io is invalid
    firFilter.clock <= clock
    firFilter.reset <= reset
    firFilter.io.taps[0] <= delays_5 @[StreamingAutocorrelator.scala 31:21]
    firFilter.io.taps[1] <= delays_4 @[StreamingAutocorrelator.scala 31:21]
    firFilter.io.taps[2] <= delays_3 @[StreamingAutocorrelator.scala 31:21]
    firFilter.io.input <= io.input @[StreamingAutocorrelator.scala 32:22]
    io.output <= firFilter.io.output @[StreamingAutocorrelator.scala 33:13]
    
