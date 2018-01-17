;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit VecPassThrough : 
  module VecPassThrough : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inVector : UInt<16>[10], outVector : UInt<16>[10], outVectorAsUInt : UInt<160>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg regVector : UInt<16>[10], clock @[AggregateOrderingSpec.scala 21:22]
    regVector[0] <= io.inVector[0] @[AggregateOrderingSpec.scala 23:13]
    regVector[1] <= io.inVector[1] @[AggregateOrderingSpec.scala 23:13]
    regVector[2] <= io.inVector[2] @[AggregateOrderingSpec.scala 23:13]
    regVector[3] <= io.inVector[3] @[AggregateOrderingSpec.scala 23:13]
    regVector[4] <= io.inVector[4] @[AggregateOrderingSpec.scala 23:13]
    regVector[5] <= io.inVector[5] @[AggregateOrderingSpec.scala 23:13]
    regVector[6] <= io.inVector[6] @[AggregateOrderingSpec.scala 23:13]
    regVector[7] <= io.inVector[7] @[AggregateOrderingSpec.scala 23:13]
    regVector[8] <= io.inVector[8] @[AggregateOrderingSpec.scala 23:13]
    regVector[9] <= io.inVector[9] @[AggregateOrderingSpec.scala 23:13]
    io.outVector[0] <= regVector[0] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[1] <= regVector[1] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[2] <= regVector[2] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[3] <= regVector[3] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[4] <= regVector[4] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[5] <= regVector[5] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[6] <= regVector[6] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[7] <= regVector[7] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[8] <= regVector[8] @[AggregateOrderingSpec.scala 24:16]
    io.outVector[9] <= regVector[9] @[AggregateOrderingSpec.scala 24:16]
    node _T_48 = cat(io.inVector[1], io.inVector[0]) @[AggregateOrderingSpec.scala 26:43]
    node _T_49 = cat(io.inVector[4], io.inVector[3]) @[AggregateOrderingSpec.scala 26:43]
    node _T_50 = cat(_T_49, io.inVector[2]) @[AggregateOrderingSpec.scala 26:43]
    node _T_51 = cat(_T_50, _T_48) @[AggregateOrderingSpec.scala 26:43]
    node _T_52 = cat(io.inVector[6], io.inVector[5]) @[AggregateOrderingSpec.scala 26:43]
    node _T_53 = cat(io.inVector[9], io.inVector[8]) @[AggregateOrderingSpec.scala 26:43]
    node _T_54 = cat(_T_53, io.inVector[7]) @[AggregateOrderingSpec.scala 26:43]
    node _T_55 = cat(_T_54, _T_52) @[AggregateOrderingSpec.scala 26:43]
    node _T_56 = cat(_T_55, _T_51) @[AggregateOrderingSpec.scala 26:43]
    io.outVectorAsUInt <= _T_56 @[AggregateOrderingSpec.scala 26:22]
    
