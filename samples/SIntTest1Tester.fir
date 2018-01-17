;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-07-28 20:29:43.516, builtAtMillis: 1501273783516
circuit SIntTest1Tester : 
  module SIntTest1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : SInt<6>, flip in2 : SInt<6>, out : SInt<8>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_5 = add(io.in1, io.in2) @[IntervalSpec.scala 68:20]
    node _T_6 = tail(_T_5, 1) @[IntervalSpec.scala 68:20]
    node _T_7 = asSInt(_T_6) @[IntervalSpec.scala 68:20]
    io.out <= _T_7 @[IntervalSpec.scala 68:10]
    
  module SIntTest1Tester : 
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
    inst dut of SIntTest1 @[IntervalSpec.scala 71:19]
    dut.io is invalid
    dut.clock <= clock
    dut.reset <= reset
    dut.io.in1 <= asSInt(UInt<4>("h04")) @[IntervalSpec.scala 73:14]
    dut.io.in2 <= asSInt(UInt<4>("h04")) @[IntervalSpec.scala 74:14]
    node _T_16 = eq(dut.io.out, asSInt(UInt<5>("h08"))) @[IntervalSpec.scala 75:21]
    node _T_17 = or(_T_16, reset) @[IntervalSpec.scala 75:9]
    node _T_19 = eq(_T_17, UInt<1>("h00")) @[IntervalSpec.scala 75:9]
    when _T_19 : @[IntervalSpec.scala 75:9]
      printf(clock, UInt<1>(1), "Assertion failed\n    at IntervalSpec.scala:75 assert(dut.io.out === 8.S)\n") @[IntervalSpec.scala 75:9]
      stop(clock, UInt<1>(1), 1) @[IntervalSpec.scala 75:9]
      skip @[IntervalSpec.scala 75:9]
    node _T_21 = eq(reset, UInt<1>("h00")) @[IntervalSpec.scala 78:7]
    when _T_21 : @[IntervalSpec.scala 78:7]
      stop(clock, UInt<1>(1), 0) @[IntervalSpec.scala 78:7]
      skip @[IntervalSpec.scala 78:7]
    
