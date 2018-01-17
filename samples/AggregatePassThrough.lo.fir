circuit AggregatePassThrough : @[:@2.0]
  module AggregatePassThrough : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_inputAggregate_u0 : UInt<4> @[:@6.4]
    input io_inputAggregate_u1 : UInt<4> @[:@6.4]
    input io_inputAggregate_u2 : UInt<4> @[:@6.4]
    input io_inputAggregate_u3 : UInt<4> @[:@6.4]
    input io_inputAggregate_u4 : UInt<4> @[:@6.4]
    output io_outputAggregate_u0 : UInt<4> @[:@6.4]
    output io_outputAggregate_u1 : UInt<4> @[:@6.4]
    output io_outputAggregate_u2 : UInt<4> @[:@6.4]
    output io_outputAggregate_u3 : UInt<4> @[:@6.4]
    output io_outputAggregate_u4 : UInt<4> @[:@6.4]
    output io_aggregateAsUInt : UInt<20> @[:@6.4]
    output io_outputFromUInt_u0 : UInt<4> @[:@6.4]
    output io_outputFromUInt_u1 : UInt<4> @[:@6.4]
    output io_outputFromUInt_u2 : UInt<4> @[:@6.4]
    output io_outputFromUInt_u3 : UInt<4> @[:@6.4]
    output io_outputFromUInt_u4 : UInt<4> @[:@6.4]
  
    reg aggregateRegister_u0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), aggregateRegister_u0) @[AggregateOrderingSpec.scala 134:30:@11.4]
    reg aggregateRegister_u1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), aggregateRegister_u1) @[AggregateOrderingSpec.scala 134:30:@11.4]
    reg aggregateRegister_u2 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), aggregateRegister_u2) @[AggregateOrderingSpec.scala 134:30:@11.4]
    reg aggregateRegister_u3 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), aggregateRegister_u3) @[AggregateOrderingSpec.scala 134:30:@11.4]
    reg aggregateRegister_u4 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), aggregateRegister_u4) @[AggregateOrderingSpec.scala 134:30:@11.4]
    node _T_8 = cat(aggregateRegister_u3, aggregateRegister_u4) @[AggregateOrderingSpec.scala 139:49:@22.4]
    node _T_9 = cat(aggregateRegister_u0, aggregateRegister_u1) @[AggregateOrderingSpec.scala 139:49:@23.4]
    node _T_10 = cat(_T_9, aggregateRegister_u2) @[AggregateOrderingSpec.scala 139:49:@24.4]
    node _T_11 = cat(_T_10, _T_8) @[AggregateOrderingSpec.scala 139:49:@25.4]
    wire _T_14_u0 : UInt<4> @[AggregateOrderingSpec.scala 140:50:@27.4]
    wire _T_14_u1 : UInt<4> @[AggregateOrderingSpec.scala 140:50:@27.4]
    wire _T_14_u2 : UInt<4> @[AggregateOrderingSpec.scala 140:50:@27.4]
    wire _T_14_u3 : UInt<4> @[AggregateOrderingSpec.scala 140:50:@27.4]
    wire _T_14_u4 : UInt<4> @[AggregateOrderingSpec.scala 140:50:@27.4]
    node _T_15 = cat(aggregateRegister_u3, aggregateRegister_u4) @[AggregateOrderingSpec.scala 140:50:@29.4]
    node _T_16 = cat(aggregateRegister_u0, aggregateRegister_u1) @[AggregateOrderingSpec.scala 140:50:@30.4]
    node _T_17 = cat(_T_16, aggregateRegister_u2) @[AggregateOrderingSpec.scala 140:50:@31.4]
    node _T_18 = cat(_T_17, _T_15) @[AggregateOrderingSpec.scala 140:50:@32.4]
    wire _T_20 : UInt<20> @[:@33.4]
    node _T_21 = bits(_T_20, 3, 0) @[AggregateOrderingSpec.scala 140:50:@36.4]
    node _T_22 = bits(_T_20, 7, 4) @[AggregateOrderingSpec.scala 140:50:@38.4]
    node _T_23 = bits(_T_20, 11, 8) @[AggregateOrderingSpec.scala 140:50:@40.4]
    node _T_24 = bits(_T_20, 15, 12) @[AggregateOrderingSpec.scala 140:50:@42.4]
    node _T_25 = bits(_T_20, 19, 16) @[AggregateOrderingSpec.scala 140:50:@44.4]
    io_outputAggregate_u0 <= aggregateRegister_u0
    io_outputAggregate_u1 <= aggregateRegister_u1
    io_outputAggregate_u2 <= aggregateRegister_u2
    io_outputAggregate_u3 <= aggregateRegister_u3
    io_outputAggregate_u4 <= aggregateRegister_u4
    io_aggregateAsUInt <= _T_11
    io_outputFromUInt_u0 <= _T_14_u0
    io_outputFromUInt_u1 <= _T_14_u1
    io_outputFromUInt_u2 <= _T_14_u2
    io_outputFromUInt_u3 <= _T_14_u3
    io_outputFromUInt_u4 <= _T_14_u4
    aggregateRegister_u0 <= io_inputAggregate_u0
    aggregateRegister_u1 <= io_inputAggregate_u1
    aggregateRegister_u2 <= io_inputAggregate_u2
    aggregateRegister_u3 <= io_inputAggregate_u3
    aggregateRegister_u4 <= io_inputAggregate_u4
    _T_14_u0 <= _T_25
    _T_14_u1 <= _T_24
    _T_14_u2 <= _T_23
    _T_14_u3 <= _T_22
    _T_14_u4 <= _T_21
    _T_20 <= _T_18
