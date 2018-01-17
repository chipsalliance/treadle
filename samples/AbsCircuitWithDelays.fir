;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit AbsCircuitWithDelays : 
  module AbsCircuitWithDelays : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<16><<8>>, outContextAbs : Fixed<16><<8>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg _T_4 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_4 <= io.in @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_6 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_6 <= _T_4 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_8 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_8 <= _T_6 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_9 = bits(_T_8, 15, 15) @[FixedPointTypeClass.scala 181:24]
    node _T_11 = sub(asFixedPoint(UInt<1>("h00"), 0), io.in) @[FixedPointTypeClass.scala 34:22]
    reg _T_14 : Fixed<17><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_14 <= _T_11 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_16 : Fixed<17><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_16 <= _T_14 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_18 : Fixed<17><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_18 <= _T_16 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_21 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_21 <= io.in @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_23 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_23 <= _T_21 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    reg _T_25 : Fixed<16><<8>>, clock @[Reg.scala 12:16]
    when UInt<1>("h01") : @[Reg.scala 13:19]
      _T_25 <= _T_23 @[Reg.scala 13:23]
      skip @[Reg.scala 13:19]
    node _T_27 = mux(_T_9, _T_18, _T_25) @[FixedPointTypeClass.scala 207:8]
    node _T_28 = asUInt(io.in) @[ShiftRegisterDelaySpec.scala 26:45]
    node _T_29 = asUInt(_T_27) @[ShiftRegisterDelaySpec.scala 26:59]
    node _T_30 = bits(reset, 0, 0) @[ShiftRegisterDelaySpec.scala 26:11]
    node _T_32 = eq(_T_30, UInt<1>("h00")) @[ShiftRegisterDelaySpec.scala 26:11]
    when _T_32 : @[ShiftRegisterDelaySpec.scala 26:11]
      printf(clock, UInt<1>(1), "io.in %d con %d\n", _T_28, _T_29) @[ShiftRegisterDelaySpec.scala 26:11]
      skip @[ShiftRegisterDelaySpec.scala 26:11]
    io.outContextAbs <= _T_27 @[ShiftRegisterDelaySpec.scala 27:22]
    
