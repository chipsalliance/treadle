;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit BlackBoxFloatTester : 
  extmodule BBFAdd : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFAdd
    
    
  extmodule BBFMultiply : 
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFMultiply
    
    
  extmodule BBFEquals : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_1 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_2 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_3 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_4 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_5 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_6 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  extmodule BBFEquals_7 : 
    output out : UInt<1>
    input in2 : UInt<64>
    input in1 : UInt<64>
    
    defname = BBFEquals
    
    
  module BlackBoxFloatTester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg value : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[Counter.scala 17:33]
    when UInt<1>("h01") : @[Counter.scala 62:17]
      node _T_6 = eq(value, UInt<4>("h09")) @[Counter.scala 25:24]
      node _T_8 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_9 = tail(_T_8, 1) @[Counter.scala 26:22]
      value <= _T_9 @[Counter.scala 26:13]
      when _T_6 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      skip @[Counter.scala 62:17]
    node _T_11 = and(UInt<1>("h01"), _T_6) @[Counter.scala 63:20]
    wire _T_13 : {node : UInt<64>} @[BlackBoxFloat.scala 15:27]
    _T_13 is invalid @[BlackBoxFloat.scala 15:27]
    reg accum : {node : UInt<64>}, clock with : (reset => (reset, _T_13)) @[BlackBoxFloat.scala 15:22]
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36]
    BBFAdd.out is invalid
    BBFAdd.in2 is invalid
    BBFAdd.in1 is invalid
    BBFAdd.in1 <= accum.node @[DspReal.scala 26:21]
    BBFAdd.in2 <= UInt<64>("h03ff0000000000000") @[DspReal.scala 27:21]
    wire addOut : {node : UInt<64>} @[DspReal.scala 28:19]
    addOut is invalid @[DspReal.scala 28:19]
    addOut.node <= BBFAdd.out @[DspReal.scala 29:14]
    inst BBFMultiply of BBFMultiply @[DspReal.scala 51:36]
    BBFMultiply.out is invalid
    BBFMultiply.in2 is invalid
    BBFMultiply.in1 is invalid
    BBFMultiply.in1 <= addOut.node @[DspReal.scala 26:21]
    BBFMultiply.in2 <= UInt<64>("h04000000000000000") @[DspReal.scala 27:21]
    wire mulOut : {node : UInt<64>} @[DspReal.scala 28:19]
    mulOut is invalid @[DspReal.scala 28:19]
    mulOut.node <= BBFMultiply.out @[DspReal.scala 29:14]
    accum.node <= addOut.node @[BlackBoxFloat.scala 20:9]
    node _T_26 = bits(reset, 0, 0) @[BlackBoxFloat.scala 22:9]
    node _T_28 = eq(_T_26, UInt<1>("h00")) @[BlackBoxFloat.scala 22:9]
    when _T_28 : @[BlackBoxFloat.scala 22:9]
      printf(clock, UInt<1>(1), "cnt: %x     accum: %x    add: %x    mult: %x\n", value, accum.node, addOut.node, mulOut.node) @[BlackBoxFloat.scala 22:9]
      skip @[BlackBoxFloat.scala 22:9]
    node _T_30 = eq(value, UInt<1>("h00")) @[BlackBoxFloat.scala 25:13]
    when _T_30 : @[BlackBoxFloat.scala 25:22]
      inst BBFEquals of BBFEquals @[DspReal.scala 75:32]
      BBFEquals.out is invalid
      BBFEquals.in2 is invalid
      BBFEquals.in1 is invalid
      BBFEquals.in1 <= addOut.node @[DspReal.scala 35:21]
      BBFEquals.in2 <= UInt<64>("h03ff0000000000000") @[DspReal.scala 36:21]
      wire _T_34 : UInt<1> @[DspReal.scala 37:19]
      _T_34 is invalid @[DspReal.scala 37:19]
      _T_34 <= BBFEquals.out @[DspReal.scala 38:9]
      node _T_35 = bits(reset, 0, 0) @[BlackBoxFloat.scala 26:11]
      node _T_36 = or(_T_34, _T_35) @[BlackBoxFloat.scala 26:11]
      node _T_38 = eq(_T_36, UInt<1>("h00")) @[BlackBoxFloat.scala 26:11]
      when _T_38 : @[BlackBoxFloat.scala 26:11]
        printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:26 assert(addOut === DspReal(1))\n") @[BlackBoxFloat.scala 26:11]
        stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 26:11]
        skip @[BlackBoxFloat.scala 26:11]
      inst BBFEquals_1 of BBFEquals_1 @[DspReal.scala 75:32]
      BBFEquals_1.out is invalid
      BBFEquals_1.in2 is invalid
      BBFEquals_1.in1 is invalid
      BBFEquals_1.in1 <= mulOut.node @[DspReal.scala 35:21]
      BBFEquals_1.in2 <= UInt<64>("h04000000000000000") @[DspReal.scala 36:21]
      wire _T_42 : UInt<1> @[DspReal.scala 37:19]
      _T_42 is invalid @[DspReal.scala 37:19]
      _T_42 <= BBFEquals_1.out @[DspReal.scala 38:9]
      node _T_43 = bits(reset, 0, 0) @[BlackBoxFloat.scala 27:11]
      node _T_44 = or(_T_42, _T_43) @[BlackBoxFloat.scala 27:11]
      node _T_46 = eq(_T_44, UInt<1>("h00")) @[BlackBoxFloat.scala 27:11]
      when _T_46 : @[BlackBoxFloat.scala 27:11]
        printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:27 assert(mulOut === DspReal(2))\n") @[BlackBoxFloat.scala 27:11]
        stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 27:11]
        skip @[BlackBoxFloat.scala 27:11]
      skip @[BlackBoxFloat.scala 25:22]
    else : @[BlackBoxFloat.scala 28:29]
      node _T_48 = eq(value, UInt<1>("h01")) @[BlackBoxFloat.scala 28:20]
      when _T_48 : @[BlackBoxFloat.scala 28:29]
        inst BBFEquals_2 of BBFEquals_2 @[DspReal.scala 75:32]
        BBFEquals_2.out is invalid
        BBFEquals_2.in2 is invalid
        BBFEquals_2.in1 is invalid
        BBFEquals_2.in1 <= addOut.node @[DspReal.scala 35:21]
        BBFEquals_2.in2 <= UInt<64>("h04000000000000000") @[DspReal.scala 36:21]
        wire _T_52 : UInt<1> @[DspReal.scala 37:19]
        _T_52 is invalid @[DspReal.scala 37:19]
        _T_52 <= BBFEquals_2.out @[DspReal.scala 38:9]
        node _T_53 = bits(reset, 0, 0) @[BlackBoxFloat.scala 29:11]
        node _T_54 = or(_T_52, _T_53) @[BlackBoxFloat.scala 29:11]
        node _T_56 = eq(_T_54, UInt<1>("h00")) @[BlackBoxFloat.scala 29:11]
        when _T_56 : @[BlackBoxFloat.scala 29:11]
          printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:29 assert(addOut === DspReal(2))\n") @[BlackBoxFloat.scala 29:11]
          stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 29:11]
          skip @[BlackBoxFloat.scala 29:11]
        inst BBFEquals_3 of BBFEquals_3 @[DspReal.scala 75:32]
        BBFEquals_3.out is invalid
        BBFEquals_3.in2 is invalid
        BBFEquals_3.in1 is invalid
        BBFEquals_3.in1 <= mulOut.node @[DspReal.scala 35:21]
        BBFEquals_3.in2 <= UInt<64>("h04010000000000000") @[DspReal.scala 36:21]
        wire _T_60 : UInt<1> @[DspReal.scala 37:19]
        _T_60 is invalid @[DspReal.scala 37:19]
        _T_60 <= BBFEquals_3.out @[DspReal.scala 38:9]
        node _T_61 = bits(reset, 0, 0) @[BlackBoxFloat.scala 30:11]
        node _T_62 = or(_T_60, _T_61) @[BlackBoxFloat.scala 30:11]
        node _T_64 = eq(_T_62, UInt<1>("h00")) @[BlackBoxFloat.scala 30:11]
        when _T_64 : @[BlackBoxFloat.scala 30:11]
          printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:30 assert(mulOut === DspReal(4))\n") @[BlackBoxFloat.scala 30:11]
          stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 30:11]
          skip @[BlackBoxFloat.scala 30:11]
        skip @[BlackBoxFloat.scala 28:29]
      else : @[BlackBoxFloat.scala 31:29]
        node _T_66 = eq(value, UInt<2>("h02")) @[BlackBoxFloat.scala 31:20]
        when _T_66 : @[BlackBoxFloat.scala 31:29]
          inst BBFEquals_4 of BBFEquals_4 @[DspReal.scala 75:32]
          BBFEquals_4.out is invalid
          BBFEquals_4.in2 is invalid
          BBFEquals_4.in1 is invalid
          BBFEquals_4.in1 <= addOut.node @[DspReal.scala 35:21]
          BBFEquals_4.in2 <= UInt<64>("h04008000000000000") @[DspReal.scala 36:21]
          wire _T_70 : UInt<1> @[DspReal.scala 37:19]
          _T_70 is invalid @[DspReal.scala 37:19]
          _T_70 <= BBFEquals_4.out @[DspReal.scala 38:9]
          node _T_71 = bits(reset, 0, 0) @[BlackBoxFloat.scala 32:11]
          node _T_72 = or(_T_70, _T_71) @[BlackBoxFloat.scala 32:11]
          node _T_74 = eq(_T_72, UInt<1>("h00")) @[BlackBoxFloat.scala 32:11]
          when _T_74 : @[BlackBoxFloat.scala 32:11]
            printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:32 assert(addOut === DspReal(3))\n") @[BlackBoxFloat.scala 32:11]
            stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 32:11]
            skip @[BlackBoxFloat.scala 32:11]
          inst BBFEquals_5 of BBFEquals_5 @[DspReal.scala 75:32]
          BBFEquals_5.out is invalid
          BBFEquals_5.in2 is invalid
          BBFEquals_5.in1 is invalid
          BBFEquals_5.in1 <= mulOut.node @[DspReal.scala 35:21]
          BBFEquals_5.in2 <= UInt<64>("h04018000000000000") @[DspReal.scala 36:21]
          wire _T_78 : UInt<1> @[DspReal.scala 37:19]
          _T_78 is invalid @[DspReal.scala 37:19]
          _T_78 <= BBFEquals_5.out @[DspReal.scala 38:9]
          node _T_79 = bits(reset, 0, 0) @[BlackBoxFloat.scala 33:11]
          node _T_80 = or(_T_78, _T_79) @[BlackBoxFloat.scala 33:11]
          node _T_82 = eq(_T_80, UInt<1>("h00")) @[BlackBoxFloat.scala 33:11]
          when _T_82 : @[BlackBoxFloat.scala 33:11]
            printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:33 assert(mulOut === DspReal(6))\n") @[BlackBoxFloat.scala 33:11]
            stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 33:11]
            skip @[BlackBoxFloat.scala 33:11]
          skip @[BlackBoxFloat.scala 31:29]
        else : @[BlackBoxFloat.scala 34:29]
          node _T_84 = eq(value, UInt<2>("h03")) @[BlackBoxFloat.scala 34:20]
          when _T_84 : @[BlackBoxFloat.scala 34:29]
            inst BBFEquals_6 of BBFEquals_6 @[DspReal.scala 75:32]
            BBFEquals_6.out is invalid
            BBFEquals_6.in2 is invalid
            BBFEquals_6.in1 is invalid
            BBFEquals_6.in1 <= addOut.node @[DspReal.scala 35:21]
            BBFEquals_6.in2 <= UInt<64>("h04010000000000000") @[DspReal.scala 36:21]
            wire _T_88 : UInt<1> @[DspReal.scala 37:19]
            _T_88 is invalid @[DspReal.scala 37:19]
            _T_88 <= BBFEquals_6.out @[DspReal.scala 38:9]
            node _T_89 = bits(reset, 0, 0) @[BlackBoxFloat.scala 35:11]
            node _T_90 = or(_T_88, _T_89) @[BlackBoxFloat.scala 35:11]
            node _T_92 = eq(_T_90, UInt<1>("h00")) @[BlackBoxFloat.scala 35:11]
            when _T_92 : @[BlackBoxFloat.scala 35:11]
              printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:35 assert(addOut === DspReal(4))\n") @[BlackBoxFloat.scala 35:11]
              stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 35:11]
              skip @[BlackBoxFloat.scala 35:11]
            inst BBFEquals_7 of BBFEquals_7 @[DspReal.scala 75:32]
            BBFEquals_7.out is invalid
            BBFEquals_7.in2 is invalid
            BBFEquals_7.in1 is invalid
            BBFEquals_7.in1 <= mulOut.node @[DspReal.scala 35:21]
            BBFEquals_7.in2 <= UInt<64>("h04020000000000000") @[DspReal.scala 36:21]
            wire _T_96 : UInt<1> @[DspReal.scala 37:19]
            _T_96 is invalid @[DspReal.scala 37:19]
            _T_96 <= BBFEquals_7.out @[DspReal.scala 38:9]
            node _T_97 = bits(reset, 0, 0) @[BlackBoxFloat.scala 36:11]
            node _T_98 = or(_T_96, _T_97) @[BlackBoxFloat.scala 36:11]
            node _T_100 = eq(_T_98, UInt<1>("h00")) @[BlackBoxFloat.scala 36:11]
            when _T_100 : @[BlackBoxFloat.scala 36:11]
              printf(clock, UInt<1>(1), "Assertion failed\n    at BlackBoxFloat.scala:36 assert(mulOut === DspReal(8))\n") @[BlackBoxFloat.scala 36:11]
              stop(clock, UInt<1>(1), 1) @[BlackBoxFloat.scala 36:11]
              skip @[BlackBoxFloat.scala 36:11]
            skip @[BlackBoxFloat.scala 34:29]
    node _T_102 = geq(value, UInt<2>("h03")) @[BlackBoxFloat.scala 39:13]
    when _T_102 : @[BlackBoxFloat.scala 39:21]
      node _T_103 = bits(reset, 0, 0) @[BlackBoxFloat.scala 41:9]
      node _T_105 = eq(_T_103, UInt<1>("h00")) @[BlackBoxFloat.scala 41:9]
      when _T_105 : @[BlackBoxFloat.scala 41:9]
        stop(clock, UInt<1>(1), 0) @[BlackBoxFloat.scala 41:9]
        skip @[BlackBoxFloat.scala 41:9]
      skip @[BlackBoxFloat.scala 39:21]
    
