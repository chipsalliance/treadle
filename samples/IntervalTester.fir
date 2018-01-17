;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-07-28 20:29:43.516, builtAtMillis: 1501273783516
circuit IntervalTester : 
  module IntervalTest1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : Interval[0, 4].3, flip in2 : Interval[0, 4].3, out : Interval[0, 8].3}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.in1, io.in2) @[IntervalSpec.scala 20:20]
    io.out <= _T_5 @[IntervalSpec.scala 20:10]
    
  module IntervalTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_6 = eq(value, UInt<4>("h0a")) @[Counter.scala 25:24]
      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_9 = tail(_T_8, 1) @[Counter.scala 26:22]
      value <= _T_9 @[Counter.scala 26:13]
      when _T_6 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      skip @[Counter.scala 62:17]
    node done = and(UInt<1>("h01"), _T_6) @[Counter.scala 63:20]
    when done : @[CookbookSpec.scala 19:15]
      node _T_12 = eq(reset, UInt<1>("h00")) @[CookbookSpec.scala 19:21]
      when _T_12 : @[CookbookSpec.scala 19:21]
        stop(clock, UInt<1>(1), 0) @[CookbookSpec.scala 19:21]
        skip @[CookbookSpec.scala 19:21]
      skip @[CookbookSpec.scala 19:15]
    inst dut of IntervalTest1 @[IntervalSpec.scala 25:19]
    dut.io is invalid
    dut.clock <= clock
    dut.reset <= reset
    dut.io.in1 <= asInterval(asSInt(UInt<4>("h04")), 4, 4) @[IntervalSpec.scala 27:14]
    dut.io.in2 <= asInterval(asSInt(UInt<4>("h04")), 4, 4) @[IntervalSpec.scala 28:14]
    node _T_15 = asUInt(dut.io.out) @[IntervalSpec.scala 29:41]
    node _T_17 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 29:9]
    when _T_17 : @[IntervalSpec.scala 29:9]
      printf(clock, UInt<1>(1), "dut.io.out: %b\n", _T_15) @[IntervalSpec.scala 29:9]
      skip @[IntervalSpec.scala 29:9]
    node _T_19 = eq(dut.io.out, asInterval(asSInt(UInt<5>("h08")), 8, 8)) @[IntervalSpec.scala 30:21]
    node _T_20 = or(_T_19, reset) @[IntervalSpec.scala 30:9]
    node _T_22 = eq(_T_20, UInt<1>("h00")) @[IntervalSpec.scala 30:9]
    when _T_22 : @[IntervalSpec.scala 30:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:30 assert(dut.io.out === 8.I())\n") @[IntervalSpec.scala 30:9]
      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 30:9]
      skip @[IntervalSpec.scala 30:9]
    node _T_24 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 33:7]
    when _T_24 : @[IntervalSpec.scala 33:7]
      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 33:7]
      skip @[IntervalSpec.scala 33:7]
    
