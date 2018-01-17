;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit BundlePassThrough : 
  module BundlePassThrough : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inBundle : {u1 : UInt<3>, u2 : UInt<9>, u3 : UInt<27>}, outBundle : {u1 : UInt<3>, u2 : UInt<9>, u3 : UInt<27>}, outBundleAsUInt : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg regBundle : {u1 : UInt<3>, u2 : UInt<9>, u3 : UInt<27>}, clock @[AggregateOrderingSpec.scala 40:22]
    regBundle.u3 <= io.inBundle.u3 @[AggregateOrderingSpec.scala 42:13]
    regBundle.u2 <= io.inBundle.u2 @[AggregateOrderingSpec.scala 42:13]
    regBundle.u1 <= io.inBundle.u1 @[AggregateOrderingSpec.scala 42:13]
    io.outBundle.u3 <= regBundle.u3 @[AggregateOrderingSpec.scala 43:16]
    io.outBundle.u2 <= regBundle.u2 @[AggregateOrderingSpec.scala 43:16]
    io.outBundle.u1 <= regBundle.u1 @[AggregateOrderingSpec.scala 43:16]
    node _T_6 = cat(io.inBundle.u1, io.inBundle.u2) @[AggregateOrderingSpec.scala 45:43]
    node _T_7 = cat(_T_6, io.inBundle.u3) @[AggregateOrderingSpec.scala 45:43]
    io.outBundleAsUInt <= _T_7 @[AggregateOrderingSpec.scala 45:22]
    
