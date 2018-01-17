;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit HelloTester : 
  module Hello : 
    input clock : Clock
    input reset : UInt<1>
    output io : {out : UInt<8>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.out <= UInt<6>("h02a") @[Hello.scala 13:10]
    
  module HelloTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst device_under_test of Hello @[Hello.scala 17:33]
    device_under_test.io is invalid
    device_under_test.clock <= clock
    device_under_test.reset <= reset
    reg value : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    reg _T_6 : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[SteppedHWIOTester.scala 177:33]
    node _T_8 = eq(_T_6, UInt<1>("h00")) @[SteppedHWIOTester.scala 179:10]
    when _T_8 : @[SteppedHWIOTester.scala 179:17]
      wire _T_12 : UInt<8> @[SteppedHWIOTester.scala 135:39]
      _T_12 is invalid @[SteppedHWIOTester.scala 135:39]
      _T_12 <= UInt<1>("h00") @[SteppedHWIOTester.scala 135:39]
      wire _T_16 : UInt<8> @[SteppedHWIOTester.scala 135:39]
      _T_16 is invalid @[SteppedHWIOTester.scala 135:39]
      _T_16 <= UInt<6>("h02a") @[SteppedHWIOTester.scala 135:39]
      wire _T_19 : UInt<8>[2] @[SteppedHWIOTester.scala 133:28]
      _T_19 is invalid @[SteppedHWIOTester.scala 133:28]
      _T_19[0] <= _T_12 @[SteppedHWIOTester.scala 133:28]
      _T_19[1] <= _T_16 @[SteppedHWIOTester.scala 133:28]
      wire _T_28 : UInt<1>[2] @[SteppedHWIOTester.scala 138:39]
      _T_28 is invalid @[SteppedHWIOTester.scala 138:39]
      _T_28[0] <= UInt<1>("h00") @[SteppedHWIOTester.scala 138:39]
      _T_28[1] <= UInt<1>("h01") @[SteppedHWIOTester.scala 138:39]
      when _T_28[value] : @[SteppedHWIOTester.scala 144:51]
        node _T_35 = eq(device_under_test.io.out, _T_19[value]) @[SteppedHWIOTester.scala 145:31]
        when _T_35 : @[SteppedHWIOTester.scala 145:72]
          skip @[SteppedHWIOTester.scala 145:72]
        node _T_37 = eq(_T_35, UInt<1>("h00")) @[SteppedHWIOTester.scala 145:72]
        when _T_37 : @[SteppedHWIOTester.scala 150:19]
          node _T_40 = eq(reset, UInt<1>("h00")) @[SteppedHWIOTester.scala 151:15]
          when _T_40 : @[SteppedHWIOTester.scala 151:15]
            printf(clock, UInt<1>(1), "    failed on step %d -- port out:  %d expected %d\n", value, device_under_test.io.out, _T_19[value]) @[SteppedHWIOTester.scala 151:15]
            skip @[SteppedHWIOTester.scala 151:15]
          node _T_42 = or(UInt<1>("h00"), reset) @[SteppedHWIOTester.scala 158:15]
          node _T_44 = eq(_T_42, UInt<1>("h00")) @[SteppedHWIOTester.scala 158:15]
          when _T_44 : @[SteppedHWIOTester.scala 158:15]
            printf(clock, UInt<1>(1), "Assertion failed\n    at SteppedHWIOTester.scala:158 assert(false.B)\n") @[SteppedHWIOTester.scala 158:15]
            stop(clock, UInt<1>(1), 1) @[SteppedHWIOTester.scala 158:15]
            skip @[SteppedHWIOTester.scala 158:15]
          node _T_46 = eq(reset, UInt<1>("h00")) @[SteppedHWIOTester.scala 159:13]
          when _T_46 : @[SteppedHWIOTester.scala 159:13]
            stop(clock, UInt<1>(1), 0) @[SteppedHWIOTester.scala 159:13]
            skip @[SteppedHWIOTester.scala 159:13]
          skip @[SteppedHWIOTester.scala 150:19]
        skip @[SteppedHWIOTester.scala 144:51]
      node _T_48 = eq(value, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_50 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_51 = tail(_T_50, 1) @[Counter.scala 26:22]
      value <= _T_51 @[Counter.scala 26:13]
      when _T_48 : @[SteppedHWIOTester.scala 183:22]
        node _T_53 = eq(reset, UInt<1>("h00")) @[SteppedHWIOTester.scala 184:15]
        when _T_53 : @[SteppedHWIOTester.scala 184:15]
          printf(clock, UInt<1>(1), "Stopping, end of tests, 2 steps\n") @[SteppedHWIOTester.scala 184:15]
          skip @[SteppedHWIOTester.scala 184:15]
        _T_6 <= UInt<1>("h01") @[SteppedHWIOTester.scala 185:14]
        node _T_56 = eq(reset, UInt<1>("h00")) @[SteppedHWIOTester.scala 186:13]
        when _T_56 : @[SteppedHWIOTester.scala 186:13]
          stop(clock, UInt<1>(1), 0) @[SteppedHWIOTester.scala 186:13]
          skip @[SteppedHWIOTester.scala 186:13]
        skip @[SteppedHWIOTester.scala 183:22]
      skip @[SteppedHWIOTester.scala 179:17]
    
