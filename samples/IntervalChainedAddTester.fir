;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-07-28 20:29:43.516, builtAtMillis: 1501273783516
circuit IntervalChainedAddTester : 
  module IntervalChainedAddTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire intervalResult : Interval.0 @[IntervalSpec.scala 102:28]
    intervalResult is invalid @[IntervalSpec.scala 102:28]
    wire uintResult : UInt @[IntervalSpec.scala 103:24]
    uintResult is invalid @[IntervalSpec.scala 103:24]
    node _T_6 = add(asInterval(asSInt(UInt<3>("h02")), 2, 2), asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:25]
    node _T_8 = add(_T_6, asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:31]
    node _T_10 = add(_T_8, asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:37]
    node _T_12 = add(_T_10, asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:43]
    node _T_14 = add(_T_12, asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:49]
    node _T_16 = add(_T_14, asInterval(asSInt(UInt<3>("h02")), 2, 2)) @[IntervalSpec.scala 105:55]
    intervalResult <= _T_16 @[IntervalSpec.scala 105:18]
    node _T_19 = add(UInt<2>("h02"), UInt<2>("h02")) @[IntervalSpec.scala 106:21]
    node _T_21 = add(_T_19, UInt<2>("h02")) @[IntervalSpec.scala 106:28]
    node _T_23 = add(_T_21, UInt<2>("h02")) @[IntervalSpec.scala 106:35]
    node _T_25 = add(_T_23, UInt<2>("h02")) @[IntervalSpec.scala 106:42]
    node _T_27 = add(_T_25, UInt<2>("h02")) @[IntervalSpec.scala 106:49]
    node _T_29 = add(_T_27, UInt<2>("h02")) @[IntervalSpec.scala 106:56]
    uintResult <= _T_29 @[IntervalSpec.scala 106:14]
    node _T_30 = asUInt(intervalResult) @[IntervalSpec.scala 108:50]
    node _T_32 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 108:9]
    when _T_32 : @[IntervalSpec.scala 108:9]
      printf(clock, UInt<1>(1), "Interval result: %d\n", _T_30) @[IntervalSpec.scala 108:9]
      skip @[IntervalSpec.scala 108:9]
    node _T_34 = eq(intervalResult, asInterval(asSInt(UInt<5>("h0e")), 14, 14)) @[IntervalSpec.scala 109:25]
    node _T_35 = or(_T_34, reset) @[IntervalSpec.scala 109:9]
    node _T_37 = eq(_T_35, UInt<1>("h00")) @[IntervalSpec.scala 109:9]
    when _T_37 : @[IntervalSpec.scala 109:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:109 assert(intervalResult === 14.I)\n") @[IntervalSpec.scala 109:9]
      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 109:9]
      skip @[IntervalSpec.scala 109:9]
    node _T_39 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 110:9]
    when _T_39 : @[IntervalSpec.scala 110:9]
      printf(clock, UInt<1>(1), "UInt result: %d\n", uintResult) @[IntervalSpec.scala 110:9]
      skip @[IntervalSpec.scala 110:9]
    node _T_41 = eq(uintResult, UInt<4>("h0e")) @[IntervalSpec.scala 111:21]
    node _T_42 = or(_T_41, reset) @[IntervalSpec.scala 111:9]
    node _T_44 = eq(_T_42, UInt<1>("h00")) @[IntervalSpec.scala 111:9]
    when _T_44 : @[IntervalSpec.scala 111:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:111 assert(uintResult === 14.U)\n") @[IntervalSpec.scala 111:9]
      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 111:9]
      skip @[IntervalSpec.scala 111:9]
    node _T_46 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 112:7]
    when _T_46 : @[IntervalSpec.scala 112:7]
      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 112:7]
      skip @[IntervalSpec.scala 112:7]
    
