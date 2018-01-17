circuit VecPassThrough : @[:@2.0]
  module VecPassThrough : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_inVector_0 : UInt<16> @[:@6.4]
    input io_inVector_1 : UInt<16> @[:@6.4]
    input io_inVector_2 : UInt<16> @[:@6.4]
    input io_inVector_3 : UInt<16> @[:@6.4]
    input io_inVector_4 : UInt<16> @[:@6.4]
    input io_inVector_5 : UInt<16> @[:@6.4]
    input io_inVector_6 : UInt<16> @[:@6.4]
    input io_inVector_7 : UInt<16> @[:@6.4]
    input io_inVector_8 : UInt<16> @[:@6.4]
    input io_inVector_9 : UInt<16> @[:@6.4]
    output io_outVector_0 : UInt<16> @[:@6.4]
    output io_outVector_1 : UInt<16> @[:@6.4]
    output io_outVector_2 : UInt<16> @[:@6.4]
    output io_outVector_3 : UInt<16> @[:@6.4]
    output io_outVector_4 : UInt<16> @[:@6.4]
    output io_outVector_5 : UInt<16> @[:@6.4]
    output io_outVector_6 : UInt<16> @[:@6.4]
    output io_outVector_7 : UInt<16> @[:@6.4]
    output io_outVector_8 : UInt<16> @[:@6.4]
    output io_outVector_9 : UInt<16> @[:@6.4]
    output io_outVectorAsUInt : UInt<160> @[:@6.4]
  
    reg regVector_0 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_0) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_1 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_1) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_2 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_2) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_3 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_3) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_4 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_4) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_5 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_5) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_6 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_6) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_7 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_7) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_8 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_8) @[AggregateOrderingSpec.scala 21:22:@11.4]
    reg regVector_9 : UInt<16>, clock with :
      reset => (UInt<1>("h0"), regVector_9) @[AggregateOrderingSpec.scala 21:22:@11.4]
    node _T_48 = cat(io_inVector_1, io_inVector_0) @[AggregateOrderingSpec.scala 26:43:@32.4]
    node _T_49 = cat(io_inVector_4, io_inVector_3) @[AggregateOrderingSpec.scala 26:43:@33.4]
    node _T_50 = cat(_T_49, io_inVector_2) @[AggregateOrderingSpec.scala 26:43:@34.4]
    node _T_51 = cat(_T_50, _T_48) @[AggregateOrderingSpec.scala 26:43:@35.4]
    node _T_52 = cat(io_inVector_6, io_inVector_5) @[AggregateOrderingSpec.scala 26:43:@36.4]
    node _T_53 = cat(io_inVector_9, io_inVector_8) @[AggregateOrderingSpec.scala 26:43:@37.4]
    node _T_54 = cat(_T_53, io_inVector_7) @[AggregateOrderingSpec.scala 26:43:@38.4]
    node _T_55 = cat(_T_54, _T_52) @[AggregateOrderingSpec.scala 26:43:@39.4]
    node _T_56 = cat(_T_55, _T_51) @[AggregateOrderingSpec.scala 26:43:@40.4]
    io_outVector_0 <= regVector_0
    io_outVector_1 <= regVector_1
    io_outVector_2 <= regVector_2
    io_outVector_3 <= regVector_3
    io_outVector_4 <= regVector_4
    io_outVector_5 <= regVector_5
    io_outVector_6 <= regVector_6
    io_outVector_7 <= regVector_7
    io_outVector_8 <= regVector_8
    io_outVector_9 <= regVector_9
    io_outVectorAsUInt <= _T_56
    regVector_0 <= io_inVector_0
    regVector_1 <= io_inVector_1
    regVector_2 <= io_inVector_2
    regVector_3 <= io_inVector_3
    regVector_4 <= io_inVector_4
    regVector_5 <= io_inVector_5
    regVector_6 <= io_inVector_6
    regVector_7 <= io_inVector_7
    regVector_8 <= io_inVector_8
    regVector_9 <= io_inVector_9
