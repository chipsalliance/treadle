;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit SmallOdds4Tester : 
  module SmallOdds4Filter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.in.ready <= io.out.ready @[SmallOdds4.scala 19:17]
    io.out.bits <= io.in.bits @[SmallOdds4.scala 20:17]
    node _T_14 = and(io.out.ready, io.in.valid) @[SmallOdds4.scala 22:34]
    node _T_16 = lt(io.in.bits, UInt<4>("h0a")) @[SmallOdds4.scala 28:38]
    node _T_17 = and(_T_14, _T_16) @[SmallOdds4.scala 22:49]
    io.out.valid <= _T_17 @[SmallOdds4.scala 22:18]
    
  module Queue : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip enq : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, deq : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, count : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    cmem ram : UInt<32>[1] @[Decoupled.scala 200:24]
    reg maybe_full : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Decoupled.scala 203:35]
    node _T_24 = eq(UInt<1>("h00"), UInt<1>("h00")) @[Decoupled.scala 205:41]
    node _T_26 = eq(maybe_full, UInt<1>("h00")) @[Decoupled.scala 206:36]
    node _T_27 = and(_T_24, _T_26) @[Decoupled.scala 206:33]
    node _T_28 = and(_T_24, maybe_full) @[Decoupled.scala 207:32]
    node _T_29 = and(io.enq.ready, io.enq.valid) @[Decoupled.scala 29:37]
    wire do_enq : UInt<1>
    do_enq is invalid
    do_enq <= _T_29
    node _T_31 = and(io.deq.ready, io.deq.valid) @[Decoupled.scala 29:37]
    wire do_deq : UInt<1>
    do_deq is invalid
    do_deq <= _T_31
    when do_enq : @[Decoupled.scala 211:17]
      infer mport _T_34 = ram[UInt<1>("h00")], clock
      _T_34 <= io.enq.bits @[Decoupled.scala 212:24]
      skip @[Decoupled.scala 211:17]
    when do_deq : @[Decoupled.scala 215:17]
      skip @[Decoupled.scala 215:17]
    node _T_37 = neq(do_enq, do_deq) @[Decoupled.scala 218:16]
    when _T_37 : @[Decoupled.scala 218:27]
      maybe_full <= do_enq @[Decoupled.scala 219:16]
      skip @[Decoupled.scala 218:27]
    node _T_39 = eq(_T_27, UInt<1>("h00")) @[Decoupled.scala 222:19]
    io.deq.valid <= _T_39 @[Decoupled.scala 222:16]
    node _T_41 = eq(_T_28, UInt<1>("h00")) @[Decoupled.scala 223:19]
    io.enq.ready <= _T_41 @[Decoupled.scala 223:16]
    infer mport _T_43 = ram[UInt<1>("h00")], clock
    io.deq.bits <= _T_43 @[Decoupled.scala 224:15]
    node _T_44 = sub(UInt<1>("h00"), UInt<1>("h00")) @[Decoupled.scala 239:40]
    node _T_45 = asUInt(_T_44) @[Decoupled.scala 239:40]
    node _T_46 = tail(_T_45, 1) @[Decoupled.scala 239:40]
    node _T_47 = and(maybe_full, _T_24) @[Decoupled.scala 241:32]
    node _T_48 = cat(_T_47, _T_46) @[Cat.scala 30:58]
    io.count <= _T_48 @[Decoupled.scala 241:14]
    
  module SmallOdds4Filter_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.in.ready <= io.out.ready @[SmallOdds4.scala 19:17]
    io.out.bits <= io.in.bits @[SmallOdds4.scala 20:17]
    node _T_14 = and(io.out.ready, io.in.valid) @[SmallOdds4.scala 22:34]
    node _T_16 = and(io.in.bits, UInt<1>("h01")) @[SmallOdds4.scala 30:52]
    node _T_18 = eq(_T_16, UInt<1>("h01")) @[SmallOdds4.scala 30:59]
    node _T_19 = and(_T_14, _T_18) @[SmallOdds4.scala 22:49]
    io.out.valid <= _T_19 @[SmallOdds4.scala 22:18]
    
  module SmallOdds4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst SmallOdds4Filter of SmallOdds4Filter @[SmallOdds4.scala 28:24]
    SmallOdds4Filter.io is invalid
    SmallOdds4Filter.clock <= clock
    SmallOdds4Filter.reset <= reset
    inst Queue of Queue @[SmallOdds4.scala 29:24]
    Queue.io is invalid
    Queue.clock <= clock
    Queue.reset <= reset
    inst SmallOdds4Filter_1 of SmallOdds4Filter_1 @[SmallOdds4.scala 30:24]
    SmallOdds4Filter_1.io is invalid
    SmallOdds4Filter_1.clock <= clock
    SmallOdds4Filter_1.reset <= reset
    io.in.ready <= SmallOdds4Filter.io.in.ready @[SmallOdds4.scala 32:18]
    SmallOdds4Filter.io.in.bits <= io.in.bits @[SmallOdds4.scala 33:18]
    SmallOdds4Filter.io.in.valid <= io.in.valid @[SmallOdds4.scala 33:18]
    io.in.ready <= SmallOdds4Filter.io.in.ready @[SmallOdds4.scala 33:18]
    Queue.io.enq.bits <= SmallOdds4Filter.io.out.bits @[SmallOdds4.scala 34:18]
    Queue.io.enq.valid <= SmallOdds4Filter.io.out.valid @[SmallOdds4.scala 34:18]
    SmallOdds4Filter.io.out.ready <= Queue.io.enq.ready @[SmallOdds4.scala 34:18]
    SmallOdds4Filter_1.io.in.bits <= Queue.io.deq.bits @[SmallOdds4.scala 35:18]
    SmallOdds4Filter_1.io.in.valid <= Queue.io.deq.valid @[SmallOdds4.scala 35:18]
    Queue.io.deq.ready <= SmallOdds4Filter_1.io.in.ready @[SmallOdds4.scala 35:18]
    io.out.bits <= SmallOdds4Filter_1.io.out.bits @[SmallOdds4.scala 36:18]
    io.out.valid <= SmallOdds4Filter_1.io.out.valid @[SmallOdds4.scala 36:18]
    SmallOdds4Filter_1.io.out.ready <= io.out.ready @[SmallOdds4.scala 36:18]
    
  module SmallOdds4Tester : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst device_under_test of SmallOdds4 @[SmallOdds4.scala 43:33]
    device_under_test.io is invalid
    device_under_test.clock <= clock
    device_under_test.reset <= reset
    reg _T_4 : UInt<7>, clock with : (reset => (reset, UInt<7>("h00"))) @[OrderedDecoupledHWIOTester.scala 374:30]
    reg _T_7 : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[OrderedDecoupledHWIOTester.scala 375:30]
    reg _T_10 : UInt<5>, clock with : (reset => (reset, UInt<5>("h00"))) @[OrderedDecoupledHWIOTester.scala 374:30]
    reg _T_13 : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[OrderedDecoupledHWIOTester.scala 375:30]
    node _T_14 = and(_T_7, _T_13) @[OrderedDecoupledHWIOTester.scala 402:42]
    when _T_14 : @[OrderedDecoupledHWIOTester.scala 402:79]
      node _T_16 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 403:13]
      when _T_16 : @[OrderedDecoupledHWIOTester.scala 403:13]
        printf(clock, UInt<1>(1), "All input and output events completed\n") @[OrderedDecoupledHWIOTester.scala 403:13]
        skip @[OrderedDecoupledHWIOTester.scala 403:13]
      node _T_18 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 404:11]
      when _T_18 : @[OrderedDecoupledHWIOTester.scala 404:11]
        stop(clock, UInt<1>(1), 0) @[OrderedDecoupledHWIOTester.scala 404:11]
        skip @[OrderedDecoupledHWIOTester.scala 404:11]
      skip @[OrderedDecoupledHWIOTester.scala 402:79]
    reg _T_21 : UInt<12>, clock with : (reset => (reset, UInt<12>("h00"))) @[OrderedDecoupledHWIOTester.scala 407:21]
    node _T_23 = add(_T_21, UInt<1>("h01")) @[OrderedDecoupledHWIOTester.scala 408:14]
    node _T_24 = tail(_T_23, 1) @[OrderedDecoupledHWIOTester.scala 408:14]
    _T_21 <= _T_24 @[OrderedDecoupledHWIOTester.scala 408:8]
    node _T_26 = gt(_T_21, UInt<12>("h0fa0")) @[OrderedDecoupledHWIOTester.scala 409:13]
    when _T_26 : @[OrderedDecoupledHWIOTester.scala 409:67]
      node _T_29 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 410:13]
      when _T_29 : @[OrderedDecoupledHWIOTester.scala 410:13]
        printf(clock, UInt<1>(1), "Exceeded maximum allowed %d ticks in OrderedDecoupledHWIOTester, If you think code is correct use:\nDecoupleTester.max_tick_count = <some-higher-value>\nin the OrderedDecoupledHWIOTester subclass\n", UInt<12>("h0fa0")) @[OrderedDecoupledHWIOTester.scala 410:13]
        skip @[OrderedDecoupledHWIOTester.scala 410:13]
      node _T_31 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 416:11]
      when _T_31 : @[OrderedDecoupledHWIOTester.scala 416:11]
        stop(clock, UInt<1>(1), 0) @[OrderedDecoupledHWIOTester.scala 416:11]
        skip @[OrderedDecoupledHWIOTester.scala 416:11]
      skip @[OrderedDecoupledHWIOTester.scala 409:67]
    wire _T_66 : UInt<1>[32] @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66 is invalid @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[0] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[1] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[2] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[3] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[4] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[5] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[6] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[7] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[8] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[9] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[10] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[11] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[12] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[13] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[14] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[15] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[16] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[17] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[18] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[19] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[20] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[21] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[22] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[23] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[24] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[25] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[26] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[27] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[28] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[29] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[30] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_66[31] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    reg value : UInt<5>, clock with : (reset => (reset, UInt<5>("h00"))) @[Counter.scala 17:33]
    wire _T_137 : UInt<5>[32] @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137 is invalid @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[0] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[1] <= UInt<4>("h08") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[2] <= UInt<3>("h04") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[3] <= UInt<3>("h07") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[4] <= UInt<4>("h0c") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[5] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[6] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[7] <= UInt<2>("h02") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[8] <= UInt<4>("h0c") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[9] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[10] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[11] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[12] <= UInt<5>("h011") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[13] <= UInt<3>("h07") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[14] <= UInt<2>("h02") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[15] <= UInt<2>("h02") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[16] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[17] <= UInt<4>("h0d") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[18] <= UInt<5>("h012") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[19] <= UInt<4>("h0f") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[20] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[21] <= UInt<4>("h0f") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[22] <= UInt<4>("h0f") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[23] <= UInt<4>("h0a") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[24] <= UInt<5>("h012") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[25] <= UInt<4>("h0b") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[26] <= UInt<4>("h0e") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[27] <= UInt<5>("h010") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[28] <= UInt<4>("h0c") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[29] <= UInt<2>("h02") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[30] <= UInt<5>("h013") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_137[31] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 252:18]
    device_under_test.io.in.bits <= _T_137[value] @[OrderedDecoupledHWIOTester.scala 283:14]
    node _T_174 = bits(_T_4, 4, 0)
    device_under_test.io.in.valid <= _T_66[_T_174] @[OrderedDecoupledHWIOTester.scala 285:30]
    node _T_175 = and(device_under_test.io.in.valid, device_under_test.io.in.ready) @[OrderedDecoupledHWIOTester.scala 287:35]
    when _T_175 : @[OrderedDecoupledHWIOTester.scala 287:62]
      node _T_177 = eq(value, UInt<5>("h01e")) @[Counter.scala 25:24]
      node _T_179 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_180 = tail(_T_179, 1) @[Counter.scala 26:22]
      value <= _T_180 @[Counter.scala 26:13]
      when _T_177 : @[Counter.scala 28:21]
        value <= UInt<1>("h00") @[Counter.scala 28:29]
        skip @[Counter.scala 28:21]
      node _T_183 = eq(_T_7, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 380:12]
      when _T_183 : @[OrderedDecoupledHWIOTester.scala 380:28]
        node _T_185 = eq(_T_4, UInt<5>("h01e")) @[OrderedDecoupledHWIOTester.scala 381:22]
        when _T_185 : @[OrderedDecoupledHWIOTester.scala 381:48]
          _T_7 <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 382:23]
          skip @[OrderedDecoupledHWIOTester.scala 381:48]
        node _T_188 = add(_T_4, UInt<1>("h01")) @[OrderedDecoupledHWIOTester.scala 384:28]
        node _T_189 = tail(_T_188, 1) @[OrderedDecoupledHWIOTester.scala 384:28]
        _T_4 <= _T_189 @[OrderedDecoupledHWIOTester.scala 384:17]
        skip @[OrderedDecoupledHWIOTester.scala 380:28]
      skip @[OrderedDecoupledHWIOTester.scala 287:62]
    wire _T_224 : UInt<1>[32] @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224 is invalid @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[0] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[1] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[2] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[3] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[4] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[5] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[6] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[7] <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[8] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[9] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[10] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[11] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[12] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[13] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[14] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[15] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[16] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[17] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[18] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[19] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[20] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[21] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[22] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[23] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[24] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[25] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[26] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[27] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[28] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[29] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[30] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    _T_224[31] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 225:8]
    reg value_1 : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[Counter.scala 17:33]
    wire _T_272 : UInt<3>[9] @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272 is invalid @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[0] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[1] <= UInt<3>("h07") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[2] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[3] <= UInt<2>("h03") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[4] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[5] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[6] <= UInt<3>("h07") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[7] <= UInt<3>("h05") @[OrderedDecoupledHWIOTester.scala 252:18]
    _T_272[8] <= UInt<1>("h00") @[OrderedDecoupledHWIOTester.scala 252:18]
    device_under_test.io.out.ready <= _T_224[_T_10] @[OrderedDecoupledHWIOTester.scala 314:30]
    node _T_285 = and(device_under_test.io.out.ready, device_under_test.io.out.valid) @[OrderedDecoupledHWIOTester.scala 316:35]
    when _T_285 : @[OrderedDecoupledHWIOTester.scala 316:62]
      node _T_288 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 318:17]
      when _T_288 : @[OrderedDecoupledHWIOTester.scala 318:17]
        printf(clock, UInt<1>(1), "output test event %d testing out.bits = %d, should be %d\n", _T_10, device_under_test.io.out.bits, _T_272[value_1]) @[OrderedDecoupledHWIOTester.scala 318:17]
        skip @[OrderedDecoupledHWIOTester.scala 318:17]
      node _T_290 = neq(device_under_test.io.out.bits, _T_272[value_1]) @[OrderedDecoupledHWIOTester.scala 321:40]
      when _T_290 : @[OrderedDecoupledHWIOTester.scala 321:103]
        node _T_293 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 322:19]
        when _T_293 : @[OrderedDecoupledHWIOTester.scala 322:19]
          printf(clock, UInt<1>(1), "Error: event %d out.bits was %d should be %d\n", _T_10, device_under_test.io.out.bits, _T_272[value_1]) @[OrderedDecoupledHWIOTester.scala 322:19]
          skip @[OrderedDecoupledHWIOTester.scala 322:19]
        node _T_295 = or(UInt<1>("h00"), reset) @[OrderedDecoupledHWIOTester.scala 324:19]
        node _T_297 = eq(_T_295, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 324:19]
        when _T_297 : @[OrderedDecoupledHWIOTester.scala 324:19]
          printf(clock, UInt<1>(1), "Assertion failed\n    at OrderedDecoupledHWIOTester.scala:324 assert(false.B)\n") @[OrderedDecoupledHWIOTester.scala 324:19]
          stop(clock, UInt<1>(1), 1) @[OrderedDecoupledHWIOTester.scala 324:19]
          skip @[OrderedDecoupledHWIOTester.scala 324:19]
        node _T_299 = eq(reset, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 325:17]
        when _T_299 : @[OrderedDecoupledHWIOTester.scala 325:17]
          stop(clock, UInt<1>(1), 0) @[OrderedDecoupledHWIOTester.scala 325:17]
          skip @[OrderedDecoupledHWIOTester.scala 325:17]
        skip @[OrderedDecoupledHWIOTester.scala 321:103]
      node _T_301 = eq(value_1, UInt<3>("h07")) @[Counter.scala 25:24]
      node _T_303 = add(value_1, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_304 = tail(_T_303, 1) @[Counter.scala 26:22]
      value_1 <= _T_304 @[Counter.scala 26:13]
      node _T_306 = eq(_T_13, UInt<1>("h00")) @[OrderedDecoupledHWIOTester.scala 380:12]
      when _T_306 : @[OrderedDecoupledHWIOTester.scala 380:28]
        node _T_308 = eq(_T_10, UInt<3>("h07")) @[OrderedDecoupledHWIOTester.scala 381:22]
        when _T_308 : @[OrderedDecoupledHWIOTester.scala 381:48]
          _T_13 <= UInt<1>("h01") @[OrderedDecoupledHWIOTester.scala 382:23]
          skip @[OrderedDecoupledHWIOTester.scala 381:48]
        node _T_311 = add(_T_10, UInt<1>("h01")) @[OrderedDecoupledHWIOTester.scala 384:28]
        node _T_312 = tail(_T_311, 1) @[OrderedDecoupledHWIOTester.scala 384:28]
        _T_10 <= _T_312 @[OrderedDecoupledHWIOTester.scala 384:17]
        skip @[OrderedDecoupledHWIOTester.scala 380:28]
      skip @[OrderedDecoupledHWIOTester.scala 316:62]
    
