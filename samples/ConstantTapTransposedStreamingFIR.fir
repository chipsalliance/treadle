;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit ConstantTapTransposedStreamingFIR : 
  module ConstantTapTransposedStreamingFIR : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input : {valid : UInt<1>, bits : SInt<10>}, output : {valid : UInt<1>, bits : SInt<16>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    node products_0 = mul(io.input.bits, asSInt(UInt<3>("h02"))) @[SIntTypeClass.scala 44:41]
    node products_1 = mul(io.input.bits, asSInt(UInt<2>("h01"))) @[SIntTypeClass.scala 44:41]
    node products_2 = mul(io.input.bits, asSInt(UInt<1>("h00"))) @[SIntTypeClass.scala 44:41]
    reg last : SInt<16>, clock @[TransposedStreamingFIR.scala 33:20]
    reg _T_16 : SInt<13>, clock @[TransposedStreamingFIR.scala 35:18]
    when io.input.valid : @[TransposedStreamingFIR.scala 36:27]
      _T_16 <= products_0 @[TransposedStreamingFIR.scala 37:11]
      skip @[TransposedStreamingFIR.scala 36:27]
    node _T_17 = add(_T_16, products_1) @[SIntTypeClass.scala 18:40]
    node _T_18 = tail(_T_17, 1) @[SIntTypeClass.scala 18:40]
    node _T_19 = asSInt(_T_18) @[SIntTypeClass.scala 18:40]
    reg _T_21 : SInt<13>, clock @[TransposedStreamingFIR.scala 35:18]
    when io.input.valid : @[TransposedStreamingFIR.scala 36:27]
      _T_21 <= _T_19 @[TransposedStreamingFIR.scala 37:11]
      skip @[TransposedStreamingFIR.scala 36:27]
    node _T_22 = add(_T_21, products_2) @[SIntTypeClass.scala 18:40]
    node _T_23 = tail(_T_22, 1) @[SIntTypeClass.scala 18:40]
    node nextLast = asSInt(_T_23) @[SIntTypeClass.scala 18:40]
    when io.input.valid : @[TransposedStreamingFIR.scala 41:24]
      last <= nextLast @[TransposedStreamingFIR.scala 42:10]
      skip @[TransposedStreamingFIR.scala 41:24]
    io.output.bits <= last @[TransposedStreamingFIR.scala 45:18]
    reg _T_25 : UInt<1>, clock @[TransposedStreamingFIR.scala 46:29]
    _T_25 <= io.input.valid @[TransposedStreamingFIR.scala 46:29]
    io.output.valid <= _T_25 @[TransposedStreamingFIR.scala 46:19]
    
