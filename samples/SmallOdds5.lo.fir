circuit SmallOdds5 : @[:@2.0]
  module SmallOdds5Filter : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    output io_in_ready : UInt<1> @[:@6.4]
    input io_in_valid : UInt<1> @[:@6.4]
    input io_in_bits : UInt<32> @[:@6.4]
    input io_out_ready : UInt<1> @[:@6.4]
    output io_out_valid : UInt<1> @[:@6.4]
    output io_out_bits : UInt<32> @[:@6.4]
  
    reg valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid) @[DecoupledAdvTester.scala 21:20:@12.4]
    reg result : UInt<32>, clock with :
      reset => (UInt<1>("h0"), result) @[DecoupledAdvTester.scala 23:21:@13.4]
    node _T_17 = lt(io_in_bits, UInt<4>("ha")) @[DecoupledAdvTester.scala 38:36:@14.4]
    node _T_18 = and(io_in_valid, _T_17) @[DecoupledAdvTester.scala 25:23:@15.4]
    node _GEN_0 = mux(_T_18, io_in_bits, result) @[DecoupledAdvTester.scala 25:44:@16.4]
    node _GEN_1 = mux(_T_18, UInt<1>("h1"), UInt<1>("h0")) @[DecoupledAdvTester.scala 25:44:@16.4]
    node _T_21 = and(io_out_ready, valid) @[DecoupledAdvTester.scala 33:34:@24.4]
    io_in_ready <= io_out_ready
    io_out_valid <= _T_21
    io_out_bits <= result
    valid <= _GEN_1
    result <= _GEN_0

  module Queue : @[:@27.2]
    input clock : Clock @[:@28.4]
    input reset : UInt<1> @[:@29.4]
    output io_enq_ready : UInt<1> @[:@30.4]
    input io_enq_valid : UInt<1> @[:@30.4]
    input io_enq_bits : UInt<32> @[:@30.4]
    input io_deq_ready : UInt<1> @[:@30.4]
    output io_deq_valid : UInt<1> @[:@30.4]
    output io_deq_bits : UInt<32> @[:@30.4]
    output io_count : UInt<2> @[:@30.4]
  
    mem ram : @[Decoupled.scala 214:24:@35.4]
      data-type => UInt<32>
      depth => 2
      read-latency => 0
      write-latency => 1
      reader => _T_50
      writer => _T_36
      read-under-write => undefined
    reg value : UInt<1>, clock with :
      reset => (UInt<1>("h0"), value) @[Counter.scala 17:33:@36.4]
    reg value_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), value_1) @[Counter.scala 17:33:@37.4]
    reg maybe_full : UInt<1>, clock with :
      reset => (UInt<1>("h0"), maybe_full) @[Decoupled.scala 217:35:@38.4]
    node _T_28 = eq(value, value_1) @[Decoupled.scala 219:41:@39.4]
    node _T_30 = eq(maybe_full, UInt<1>("h0")) @[Decoupled.scala 220:36:@40.4]
    node empty = and(_T_28, _T_30) @[Decoupled.scala 220:33:@41.4]
    node _T_31 = and(_T_28, maybe_full) @[Decoupled.scala 221:32:@42.4]
    node _T_32 = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 30:37:@43.4]
    wire do_enq : UInt<1> @[:@44.4]
    node _T_34 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 30:37:@47.4]
    wire do_deq : UInt<1> @[:@48.4]
    node wrap = eq(value, UInt<1>("h1")) @[Counter.scala 25:24:@54.6]
    node _T_39 = add(value, UInt<1>("h1")) @[Counter.scala 26:22:@55.6]
    node _T_40 = tail(_T_39, 1) @[Counter.scala 26:22:@56.6]
    node _GEN_0 = validif(do_enq, value) @[Decoupled.scala 225:17:@51.4]
    node _GEN_1 = validif(do_enq, clock) @[Decoupled.scala 225:17:@51.4]
    node _GEN_2 = mux(do_enq, UInt<1>("h1"), UInt<1>("h0")) @[Decoupled.scala 225:17:@51.4]
    node _GEN_3 = validif(do_enq, io_enq_bits) @[Decoupled.scala 225:17:@51.4]
    node _GEN_4 = mux(do_enq, _T_40, value) @[Decoupled.scala 225:17:@51.4]
    node wrap_1 = eq(value_1, UInt<1>("h1")) @[Counter.scala 25:24:@60.6]
    node _T_43 = add(value_1, UInt<1>("h1")) @[Counter.scala 26:22:@61.6]
    node _T_44 = tail(_T_43, 1) @[Counter.scala 26:22:@62.6]
    node _GEN_5 = mux(do_deq, _T_44, value_1) @[Decoupled.scala 229:17:@59.4]
    node _T_45 = neq(do_enq, do_deq) @[Decoupled.scala 232:16:@65.4]
    node _GEN_6 = mux(_T_45, do_enq, maybe_full) @[Decoupled.scala 232:27:@66.4]
    node _T_47 = eq(empty, UInt<1>("h0")) @[Decoupled.scala 236:19:@69.4]
    node _T_49 = eq(_T_31, UInt<1>("h0")) @[Decoupled.scala 237:19:@71.4]
    node _T_51 = sub(value, value_1) @[Decoupled.scala 253:40:@75.4]
    node _T_52 = asUInt(_T_51) @[Decoupled.scala 253:40:@76.4]
    node _T_53 = tail(_T_52, 1) @[Decoupled.scala 253:40:@77.4]
    node _T_54 = and(maybe_full, _T_28) @[Decoupled.scala 255:32:@78.4]
    node _T_55 = cat(_T_54, _T_53) @[Cat.scala 30:58:@79.4]
    io_enq_ready <= _T_49
    io_deq_valid <= _T_47
    io_deq_bits <= ram._T_50.data
    io_count <= _T_55
    ram._T_50.addr <= value_1 @[:@27.2]
    ram._T_50.en <= UInt<1>("h1") @[:@27.2]
    ram._T_50.clk <= clock @[:@27.2]
    ram._T_36.addr <= _GEN_0 @[:@27.2]
    ram._T_36.en <= _GEN_2 @[:@27.2]
    ram._T_36.clk <= _GEN_1 @[:@27.2]
    ram._T_36.data <= _GEN_3 @[:@27.2]
    ram._T_36.mask <= _GEN_2 @[:@27.2]
    value <= mux(reset, UInt<1>("h0"), _GEN_4)
    value_1 <= mux(reset, UInt<1>("h0"), _GEN_5)
    maybe_full <= mux(reset, UInt<1>("h0"), _GEN_6)
    do_enq <= _T_32
    do_deq <= _T_34

  module SmallOdds5Filter_1 : @[:@82.2]
    input clock : Clock @[:@83.4]
    input reset : UInt<1> @[:@84.4]
    output io_in_ready : UInt<1> @[:@85.4]
    input io_in_valid : UInt<1> @[:@85.4]
    input io_in_bits : UInt<32> @[:@85.4]
    input io_out_ready : UInt<1> @[:@85.4]
    output io_out_valid : UInt<1> @[:@85.4]
    output io_out_bits : UInt<32> @[:@85.4]
  
    reg valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid) @[DecoupledAdvTester.scala 21:20:@91.4]
    reg result : UInt<32>, clock with :
      reset => (UInt<1>("h0"), result) @[DecoupledAdvTester.scala 23:21:@92.4]
    node _T_17 = and(io_in_bits, UInt<1>("h1")) @[DecoupledAdvTester.scala 44:50:@93.4]
    node _T_19 = eq(_T_17, UInt<1>("h1")) @[DecoupledAdvTester.scala 44:57:@94.4]
    node _T_20 = and(io_in_valid, _T_19) @[DecoupledAdvTester.scala 25:23:@95.4]
    node _GEN_0 = mux(_T_20, io_in_bits, result) @[DecoupledAdvTester.scala 25:44:@96.4]
    node _GEN_1 = mux(_T_20, UInt<1>("h1"), UInt<1>("h0")) @[DecoupledAdvTester.scala 25:44:@96.4]
    node _T_23 = and(io_out_ready, valid) @[DecoupledAdvTester.scala 33:34:@104.4]
    io_in_ready <= io_out_ready
    io_out_valid <= _T_23
    io_out_bits <= result
    valid <= _GEN_1
    result <= _GEN_0

  module SmallOdds5 : @[:@107.2]
    input clock : Clock @[:@108.4]
    input reset : UInt<1> @[:@109.4]
    output io_in_ready : UInt<1> @[:@110.4]
    input io_in_valid : UInt<1> @[:@110.4]
    input io_in_bits : UInt<32> @[:@110.4]
    input io_out_ready : UInt<1> @[:@110.4]
    output io_out_valid : UInt<1> @[:@110.4]
    output io_out_bits : UInt<32> @[:@110.4]
  
    inst smalls of SmallOdds5Filter @[DecoupledAdvTester.scala 38:22:@115.4]
    inst q of Queue @[DecoupledAdvTester.scala 43:22:@119.4]
    inst odds of SmallOdds5Filter_1 @[DecoupledAdvTester.scala 44:22:@123.4]
    io_in_ready <= smalls.io_in_ready
    io_out_valid <= odds.io_out_valid
    io_out_bits <= odds.io_out_bits
    smalls.io_in_valid <= io_in_valid
    smalls.io_in_bits <= io_in_bits
    smalls.io_out_ready <= q.io_enq_ready
    smalls.clock <= clock
    smalls.reset <= reset
    q.io_enq_valid <= smalls.io_out_valid
    q.io_enq_bits <= smalls.io_out_bits
    q.io_deq_ready <= odds.io_in_ready
    q.clock <= clock
    q.reset <= reset
    odds.io_in_valid <= q.io_deq_valid
    odds.io_in_bits <= q.io_deq_bits
    odds.io_out_ready <= io_out_ready
    odds.clock <= clock
    odds.reset <= reset
