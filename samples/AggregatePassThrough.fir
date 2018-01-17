;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit AggregatePassThrough : 
  module AggregatePassThrough : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inputAggregate : UInt<4>[5], outputAggregate : UInt<4>[5], aggregateAsUInt : UInt<20>, outputFromUInt : UInt<4>[5]}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg aggregateRegister : UInt<4>[5], clock @[AggregateOrderingSpec.scala 134:30]
    aggregateRegister[0] <= io.inputAggregate[0] @[AggregateOrderingSpec.scala 136:21]
    aggregateRegister[1] <= io.inputAggregate[1] @[AggregateOrderingSpec.scala 136:21]
    aggregateRegister[2] <= io.inputAggregate[2] @[AggregateOrderingSpec.scala 136:21]
    aggregateRegister[3] <= io.inputAggregate[3] @[AggregateOrderingSpec.scala 136:21]
    aggregateRegister[4] <= io.inputAggregate[4] @[AggregateOrderingSpec.scala 136:21]
    io.outputAggregate[0] <= aggregateRegister[0] @[AggregateOrderingSpec.scala 137:22]
    io.outputAggregate[1] <= aggregateRegister[1] @[AggregateOrderingSpec.scala 137:22]
    io.outputAggregate[2] <= aggregateRegister[2] @[AggregateOrderingSpec.scala 137:22]
    io.outputAggregate[3] <= aggregateRegister[3] @[AggregateOrderingSpec.scala 137:22]
    io.outputAggregate[4] <= aggregateRegister[4] @[AggregateOrderingSpec.scala 137:22]
    node _T_46 = cat(aggregateRegister[1], aggregateRegister[0]) @[AggregateOrderingSpec.scala 139:49]
    node _T_47 = cat(aggregateRegister[4], aggregateRegister[3]) @[AggregateOrderingSpec.scala 139:49]
    node _T_48 = cat(_T_47, aggregateRegister[2]) @[AggregateOrderingSpec.scala 139:49]
    node _T_49 = cat(_T_48, _T_46) @[AggregateOrderingSpec.scala 139:49]
    io.aggregateAsUInt <= _T_49 @[AggregateOrderingSpec.scala 139:22]
    node _T_53 = cat(aggregateRegister[1], aggregateRegister[0]) @[AggregateOrderingSpec.scala 140:76]
    node _T_54 = cat(aggregateRegister[4], aggregateRegister[3]) @[AggregateOrderingSpec.scala 140:76]
    node _T_55 = cat(_T_54, aggregateRegister[2]) @[AggregateOrderingSpec.scala 140:76]
    node _T_56 = cat(_T_55, _T_53) @[AggregateOrderingSpec.scala 140:76]
    wire _T_65 : UInt<4>[5] @[AggregateOrderingSpec.scala 140:51]
    _T_65 is invalid @[AggregateOrderingSpec.scala 140:51]
    wire _T_80 : UInt<20>
    _T_80 is invalid
    _T_80 <= _T_56
    node _T_81 = bits(_T_80, 3, 0) @[AggregateOrderingSpec.scala 140:51]
    _T_65[0] <= _T_81 @[AggregateOrderingSpec.scala 140:51]
    node _T_82 = bits(_T_80, 7, 4) @[AggregateOrderingSpec.scala 140:51]
    _T_65[1] <= _T_82 @[AggregateOrderingSpec.scala 140:51]
    node _T_83 = bits(_T_80, 11, 8) @[AggregateOrderingSpec.scala 140:51]
    _T_65[2] <= _T_83 @[AggregateOrderingSpec.scala 140:51]
    node _T_84 = bits(_T_80, 15, 12) @[AggregateOrderingSpec.scala 140:51]
    _T_65[3] <= _T_84 @[AggregateOrderingSpec.scala 140:51]
    node _T_85 = bits(_T_80, 19, 16) @[AggregateOrderingSpec.scala 140:51]
    _T_65[4] <= _T_85 @[AggregateOrderingSpec.scala 140:51]
    io.outputFromUInt[0] <= _T_65[0] @[AggregateOrderingSpec.scala 140:21]
    io.outputFromUInt[1] <= _T_65[1] @[AggregateOrderingSpec.scala 140:21]
    io.outputFromUInt[2] <= _T_65[2] @[AggregateOrderingSpec.scala 140:21]
    io.outputFromUInt[3] <= _T_65[3] @[AggregateOrderingSpec.scala 140:21]
    io.outputFromUInt[4] <= _T_65[4] @[AggregateOrderingSpec.scala 140:21]
    
