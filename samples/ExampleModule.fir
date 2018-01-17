;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit ExampleModule : 
  module ExampleModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<16>}, out : {flip ready : UInt<1>, valid : UInt<1>, bits : UInt<16>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg busy : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[InterpreterVerilatorConsistencySpec.scala 20:17]
    reg in_reg : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[InterpreterVerilatorConsistencySpec.scala 21:21]
    node _T_19 = eq(busy, UInt<1>("h00")) @[InterpreterVerilatorConsistencySpec.scala 22:18]
    io.in.ready <= _T_19 @[InterpreterVerilatorConsistencySpec.scala 22:15]
    node _T_21 = eq(busy, UInt<1>("h00")) @[InterpreterVerilatorConsistencySpec.scala 24:23]
    node _T_22 = and(io.in.valid, _T_21) @[InterpreterVerilatorConsistencySpec.scala 24:20]
    when _T_22 : @[InterpreterVerilatorConsistencySpec.scala 24:30]
      in_reg <= io.in.bits @[InterpreterVerilatorConsistencySpec.scala 25:12]
      busy <= UInt<1>("h01") @[InterpreterVerilatorConsistencySpec.scala 26:10]
      skip @[InterpreterVerilatorConsistencySpec.scala 24:30]
    reg wait_counter : UInt<16>, clock with : (reset => (reset, UInt<16>("h00"))) @[InterpreterVerilatorConsistencySpec.scala 29:25]
    node _T_27 = eq(busy, UInt<1>("h00")) @[InterpreterVerilatorConsistencySpec.scala 31:23]
    node _T_28 = and(io.in.valid, _T_27) @[InterpreterVerilatorConsistencySpec.scala 31:20]
    when _T_28 : @[InterpreterVerilatorConsistencySpec.scala 31:30]
      wait_counter <= UInt<1>("h00") @[InterpreterVerilatorConsistencySpec.scala 32:18]
      skip @[InterpreterVerilatorConsistencySpec.scala 31:30]
    when busy : @[InterpreterVerilatorConsistencySpec.scala 35:14]
      node _T_30 = eq(wait_counter, UInt<3>("h05")) @[InterpreterVerilatorConsistencySpec.scala 36:23]
      when _T_30 : @[InterpreterVerilatorConsistencySpec.scala 36:40]
        io.out.bits <= in_reg @[InterpreterVerilatorConsistencySpec.scala 37:19]
        skip @[InterpreterVerilatorConsistencySpec.scala 36:40]
      node _T_32 = eq(_T_30, UInt<1>("h00")) @[InterpreterVerilatorConsistencySpec.scala 36:40]
      when _T_32 : @[InterpreterVerilatorConsistencySpec.scala 38:17]
        io.out.bits <= UInt<1>("h00") @[InterpreterVerilatorConsistencySpec.scala 39:19]
        node _T_35 = add(wait_counter, UInt<1>("h01")) @[InterpreterVerilatorConsistencySpec.scala 40:36]
        node _T_36 = tail(_T_35, 1) @[InterpreterVerilatorConsistencySpec.scala 40:36]
        wait_counter <= _T_36 @[InterpreterVerilatorConsistencySpec.scala 40:20]
        skip @[InterpreterVerilatorConsistencySpec.scala 38:17]
      skip @[InterpreterVerilatorConsistencySpec.scala 35:14]
    node _T_37 = eq(io.out.bits, in_reg) @[InterpreterVerilatorConsistencySpec.scala 44:32]
    node _T_38 = eq(wait_counter, UInt<3>("h05")) @[InterpreterVerilatorConsistencySpec.scala 44:61]
    node _T_39 = and(_T_37, _T_38) @[InterpreterVerilatorConsistencySpec.scala 44:44]
    node _T_40 = and(_T_39, busy) @[InterpreterVerilatorConsistencySpec.scala 44:78]
    io.out.valid <= _T_40 @[InterpreterVerilatorConsistencySpec.scala 44:16]
    node _T_42 = eq(reset, UInt<1>("h00")) @[InterpreterVerilatorConsistencySpec.scala 46:9]
    when _T_42 : @[InterpreterVerilatorConsistencySpec.scala 46:9]
      printf(clock, UInt<1>(1), "From printf -- in: ready %d   valid %d   value %d  -- out:  ready %d  valid %d  value %d\n", io.in.ready, io.in.valid, io.in.bits, io.out.ready, io.out.valid, io.out.bits) @[InterpreterVerilatorConsistencySpec.scala 46:9]
      skip @[InterpreterVerilatorConsistencySpec.scala 46:9]
    
