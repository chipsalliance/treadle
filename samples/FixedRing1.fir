;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit FixedRing1 : 
  module FixedRing1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : Fixed<16><<4>>, floor : Fixed<16><<4>>, ceil : Fixed<16><<4>>, isWhole : UInt<1>, round : Fixed<16><<4>>, real : {node : UInt<64>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_9 = bpset(io.in, 0) @[FixedPointTypeClass.scala 69:58]
    io.floor <= _T_9 @[FixedPointSpec.scala 23:12]
    node _T_11 = bpset(io.in, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_12 = eq(io.in, _T_11) @[FixedPointTypeClass.scala 70:40]
    node _T_14 = bpset(io.in, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_15 = bpset(io.in, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_17 = add(_T_15, asFixedPoint(UInt<2>("h01"), 0)) @[FixedPointTypeClass.scala 25:22]
    node _T_20 = mux(_T_12, _T_14, _T_17) @[FixedPointTypeClass.scala 187:8]
    io.ceil <= _T_20 @[FixedPointSpec.scala 24:11]
    node _T_21 = bpset(io.in, 0) @[FixedPointTypeClass.scala 69:58]
    node _T_22 = eq(io.in, _T_21) @[FixedPointTypeClass.scala 70:40]
    io.isWhole <= _T_22 @[FixedPointSpec.scala 25:14]
    node _T_24 = add(io.in, asFixedPoint(UInt<2>("h01"), 1)) @[FixedPointTypeClass.scala 25:22]
    node _T_26 = bpset(_T_24, 0) @[FixedPointTypeClass.scala 69:58]
    io.round <= _T_26 @[FixedPointSpec.scala 26:12]
    
