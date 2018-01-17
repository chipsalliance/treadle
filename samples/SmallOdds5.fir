;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit SmallOdds5 : 
  module SmallOdds5Filter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.in.ready <= io.out.ready @[DecoupledAdvTester.scala 19:17]
    reg valid : UInt<1>, clock @[DecoupledAdvTester.scala 21:20]
    reg result : UInt<32>, clock @[DecoupledAdvTester.scala 23:21]
    node _T_17 = lt(io.in.bits, UInt<4>("h0a")) @[DecoupledAdvTester.scala 38:36]
    node _T_18 = and(io.in.valid, _T_17) @[DecoupledAdvTester.scala 25:23]
    when _T_18 : @[DecoupledAdvTester.scala 25:44]
      result <= io.in.bits @[DecoupledAdvTester.scala 26:14]
      valid <= UInt<1>("h01") @[DecoupledAdvTester.scala 27:13]
      skip @[DecoupledAdvTester.scala 25:44]
    node _T_21 = eq(_T_18, UInt<1>("h00")) @[DecoupledAdvTester.scala 25:44]
    when _T_21 : @[DecoupledAdvTester.scala 28:7]
      valid <= UInt<1>("h00") @[DecoupledAdvTester.scala 29:13]
      skip @[DecoupledAdvTester.scala 28:7]
    io.out.bits <= result @[DecoupledAdvTester.scala 32:17]
    node _T_23 = and(io.out.ready, valid) @[DecoupledAdvTester.scala 33:34]
    io.out.valid <= _T_23 @[DecoupledAdvTester.scala 33:18]
    
  module Queue : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip enq : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, deq : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, count : UInt<2>}
    
    clock is invalid
    reset is invalid
    io is invalid
    cmem ram : UInt<32>[2] @[Decoupled.scala 200:24]
    reg value : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    reg value_1 : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Counter.scala 17:33]
    reg maybe_full : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[Decoupled.scala 203:35]
    node _T_28 = eq(value, value_1) @[Decoupled.scala 205:41]
    node _T_30 = eq(maybe_full, UInt<1>("h00")) @[Decoupled.scala 206:36]
    node _T_31 = and(_T_28, _T_30) @[Decoupled.scala 206:33]
    node _T_32 = and(_T_28, maybe_full) @[Decoupled.scala 207:32]
    node _T_33 = and(io.enq.ready, io.enq.valid) @[Decoupled.scala 29:37]
    wire do_enq : UInt<1>
    do_enq is invalid
    do_enq <= _T_33
    node _T_35 = and(io.deq.ready, io.deq.valid) @[Decoupled.scala 29:37]
    wire do_deq : UInt<1>
    do_deq is invalid
    do_deq <= _T_35
    when do_enq : @[Decoupled.scala 211:17]
      infer mport _T_37 = ram[value], clock
      _T_37 <= io.enq.bits @[Decoupled.scala 212:24]
      node wrap = eq(value, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_40 = add(value, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_41 = tail(_T_40, 1) @[Counter.scala 26:22]
      value <= _T_41 @[Counter.scala 26:13]
      skip @[Decoupled.scala 211:17]
    when do_deq : @[Decoupled.scala 215:17]
      node wrap_1 = eq(value_1, UInt<1>("h01")) @[Counter.scala 25:24]
      node _T_44 = add(value_1, UInt<1>("h01")) @[Counter.scala 26:22]
      node _T_45 = tail(_T_44, 1) @[Counter.scala 26:22]
      value_1 <= _T_45 @[Counter.scala 26:13]
      skip @[Decoupled.scala 215:17]
    node _T_46 = neq(do_enq, do_deq) @[Decoupled.scala 218:16]
    when _T_46 : @[Decoupled.scala 218:27]
      maybe_full <= do_enq @[Decoupled.scala 219:16]
      skip @[Decoupled.scala 218:27]
    node _T_48 = eq(_T_31, UInt<1>("h00")) @[Decoupled.scala 222:19]
    io.deq.valid <= _T_48 @[Decoupled.scala 222:16]
    node _T_50 = eq(_T_32, UInt<1>("h00")) @[Decoupled.scala 223:19]
    io.enq.ready <= _T_50 @[Decoupled.scala 223:16]
    infer mport _T_51 = ram[value_1], clock
    io.deq.bits <= _T_51 @[Decoupled.scala 224:15]
    node _T_52 = sub(value, value_1) @[Decoupled.scala 239:40]
    node _T_53 = asUInt(_T_52) @[Decoupled.scala 239:40]
    node _T_54 = tail(_T_53, 1) @[Decoupled.scala 239:40]
    node _T_55 = and(maybe_full, _T_28) @[Decoupled.scala 241:32]
    node _T_56 = cat(_T_55, _T_54) @[Cat.scala 30:58]
    io.count <= _T_56 @[Decoupled.scala 241:14]
    
  module SmallOdds5Filter_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.in.ready <= io.out.ready @[DecoupledAdvTester.scala 19:17]
    reg valid : UInt<1>, clock @[DecoupledAdvTester.scala 21:20]
    reg result : UInt<32>, clock @[DecoupledAdvTester.scala 23:21]
    node _T_17 = and(io.in.bits, UInt<1>("h01")) @[DecoupledAdvTester.scala 44:50]
    node _T_19 = eq(_T_17, UInt<1>("h01")) @[DecoupledAdvTester.scala 44:57]
    node _T_20 = and(io.in.valid, _T_19) @[DecoupledAdvTester.scala 25:23]
    when _T_20 : @[DecoupledAdvTester.scala 25:44]
      result <= io.in.bits @[DecoupledAdvTester.scala 26:14]
      valid <= UInt<1>("h01") @[DecoupledAdvTester.scala 27:13]
      skip @[DecoupledAdvTester.scala 25:44]
    node _T_23 = eq(_T_20, UInt<1>("h00")) @[DecoupledAdvTester.scala 25:44]
    when _T_23 : @[DecoupledAdvTester.scala 28:7]
      valid <= UInt<1>("h00") @[DecoupledAdvTester.scala 29:13]
      skip @[DecoupledAdvTester.scala 28:7]
    io.out.bits <= result @[DecoupledAdvTester.scala 32:17]
    node _T_25 = and(io.out.ready, valid) @[DecoupledAdvTester.scala 33:34]
    io.out.valid <= _T_25 @[DecoupledAdvTester.scala 33:18]
    
  module SmallOdds5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst smalls of SmallOdds5Filter @[DecoupledAdvTester.scala 38:22]
    smalls.io is invalid
    smalls.clock <= clock
    smalls.reset <= reset
    inst q of Queue @[DecoupledAdvTester.scala 43:22]
    q.io is invalid
    q.clock <= clock
    q.reset <= reset
    inst odds of SmallOdds5Filter_1 @[DecoupledAdvTester.scala 44:22]
    odds.io is invalid
    odds.clock <= clock
    odds.reset <= reset
    smalls.io.in.bits <= io.in.bits @[DecoupledAdvTester.scala 46:16]
    smalls.io.in.valid <= io.in.valid @[DecoupledAdvTester.scala 46:16]
    io.in.ready <= smalls.io.in.ready @[DecoupledAdvTester.scala 46:16]
    q.io.enq.bits <= smalls.io.out.bits @[DecoupledAdvTester.scala 47:16]
    q.io.enq.valid <= smalls.io.out.valid @[DecoupledAdvTester.scala 47:16]
    smalls.io.out.ready <= q.io.enq.ready @[DecoupledAdvTester.scala 47:16]
    odds.io.in.bits <= q.io.deq.bits @[DecoupledAdvTester.scala 48:16]
    odds.io.in.valid <= q.io.deq.valid @[DecoupledAdvTester.scala 48:16]
    q.io.deq.ready <= odds.io.in.ready @[DecoupledAdvTester.scala 48:16]
    io.out.bits <= odds.io.out.bits @[DecoupledAdvTester.scala 49:16]
    io.out.valid <= odds.io.out.valid @[DecoupledAdvTester.scala 49:16]
    odds.io.out.ready <= io.out.ready @[DecoupledAdvTester.scala 49:16]
    
