;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-07-28 20:29:43.516, builtAtMillis: 1501273783516
circuit IntervalAddTester : 
  module IntervalAddTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire in1 : Interval[0, 4].0 @[IntervalSpec.scala 84:17]
    in1 is invalid @[IntervalSpec.scala 84:17]
    wire in2 : Interval[0, 4].0 @[IntervalSpec.scala 85:17]
    in2 is invalid @[IntervalSpec.scala 85:17]
    in1 <= asInterval(asSInt(UInt<3>("h02")), 2, 2) @[IntervalSpec.scala 87:7]
    in2 <= asInterval(asSInt(UInt<3>("h02")), 2, 2) @[IntervalSpec.scala 88:7]
    node result = add(in1, in2) @[IntervalSpec.scala 90:20]
    node _T_7 = eq(result, asInterval(asSInt(UInt<4>("h04")), 4, 4)) @[IntervalSpec.scala 92:17]
    node _T_8 = or(_T_7, reset) @[IntervalSpec.scala 92:9]
    node _T_10 = eq(_T_8, UInt<1>("h00")) @[IntervalSpec.scala 92:9]
    when _T_10 : @[IntervalSpec.scala 92:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:92 assert(result === 4.I)\n") @[IntervalSpec.scala 92:9]
      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 92:9]
      skip @[IntervalSpec.scala 92:9]
    node _T_12 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 94:7]
    when _T_12 : @[IntervalSpec.scala 94:7]
      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 94:7]
      skip @[IntervalSpec.scala 94:7]
    
