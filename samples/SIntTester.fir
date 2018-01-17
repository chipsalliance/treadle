;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SIntTester : 
  module SIntTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire xcopy : SInt<5> @[DspComplexSpec.scala 51:19]
    xcopy is invalid @[DspComplexSpec.scala 51:19]
    xcopy <= asSInt(UInt<5>("h0a")) @[DspComplexSpec.scala 52:9]
    node _T_3 = eq(asSInt(UInt<5>("h0a")), xcopy) @[DspComplexSpec.scala 54:13]
    node _T_4 = bits(reset, 0, 0) @[DspComplexSpec.scala 54:9]
    node _T_5 = or(_T_3, _T_4) @[DspComplexSpec.scala 54:9]
    node _T_7 = eq(_T_5, UInt<1>("h00")) @[DspComplexSpec.scala 54:9]
    when _T_7 : @[DspComplexSpec.scala 54:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at DspComplexSpec.scala:54 assert( x === xcopy )\n") @[DspComplexSpec.scala 54:9]
      stop(clock, UInt<1>(1), 1) @[DspComplexSpec.scala 54:9]
      skip @[DspComplexSpec.scala 54:9]
    wire y : {real : SInt<3>, imag : SInt<1>} @[DspComplex.scala 30:22]
    y is invalid @[DspComplex.scala 30:22]
    y.real <= asSInt(UInt<3>("h04")) @[DspComplex.scala 31:17]
    y.imag <= asSInt(UInt<1>("h01")) @[DspComplex.scala 32:17]
    node _T_18 = eq(y.real, asSInt(UInt<3>("h04"))) @[DspComplexSpec.scala 58:19]
    node _T_19 = bits(reset, 0, 0) @[DspComplexSpec.scala 58:10]
    node _T_20 = or(_T_18, _T_19) @[DspComplexSpec.scala 58:10]
    node _T_22 = eq(_T_20, UInt<1>("h00")) @[DspComplexSpec.scala 58:10]
    when _T_22 : @[DspComplexSpec.scala 58:10]
      printf(clock, UInt<1>(1), "Assertion failed\n    at DspComplexSpec.scala:58 assert ( y.real === (-4).S)\n") @[DspComplexSpec.scala 58:10]
      stop(clock, UInt<1>(1), 1) @[DspComplexSpec.scala 58:10]
      skip @[DspComplexSpec.scala 58:10]
    node _T_24 = eq(y.imag, asSInt(UInt<1>("h01"))) @[DspComplexSpec.scala 59:18]
    node _T_25 = bits(reset, 0, 0) @[DspComplexSpec.scala 59:10]
    node _T_26 = or(_T_24, _T_25) @[DspComplexSpec.scala 59:10]
    node _T_28 = eq(_T_26, UInt<1>("h00")) @[DspComplexSpec.scala 59:10]
    when _T_28 : @[DspComplexSpec.scala 59:10]
      printf(clock, UInt<1>(1), "Assertion failed\n    at DspComplexSpec.scala:59 assert (y.imag === (-1).S)\n") @[DspComplexSpec.scala 59:10]
      stop(clock, UInt<1>(1), 1) @[DspComplexSpec.scala 59:10]
      skip @[DspComplexSpec.scala 59:10]
    node _T_29 = bits(reset, 0, 0) @[DspComplexSpec.scala 61:7]
    node _T_31 = eq(_T_29, UInt<1>("h00")) @[DspComplexSpec.scala 61:7]
    when _T_31 : @[DspComplexSpec.scala 61:7]
      stop(clock, UInt<1>(1), 0) @[DspComplexSpec.scala 61:7]
      skip @[DspComplexSpec.scala 61:7]
    
