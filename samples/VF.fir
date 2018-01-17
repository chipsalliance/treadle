;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit VF : 
  module VF : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip addr : UInt<8>, value : UInt<8>}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire vec : UInt<8>[11] @[VecFillSpec.scala 15:17]
    vec is invalid @[VecFillSpec.scala 15:17]
    wire _T_33 : UInt<4>[11] @[VecFillSpec.scala 16:13]
    _T_33 is invalid @[VecFillSpec.scala 16:13]
    _T_33[0] <= UInt<1>("h00") @[VecFillSpec.scala 16:13]
    _T_33[1] <= UInt<1>("h01") @[VecFillSpec.scala 16:13]
    _T_33[2] <= UInt<2>("h02") @[VecFillSpec.scala 16:13]
    _T_33[3] <= UInt<2>("h03") @[VecFillSpec.scala 16:13]
    _T_33[4] <= UInt<3>("h04") @[VecFillSpec.scala 16:13]
    _T_33[5] <= UInt<3>("h05") @[VecFillSpec.scala 16:13]
    _T_33[6] <= UInt<3>("h06") @[VecFillSpec.scala 16:13]
    _T_33[7] <= UInt<3>("h07") @[VecFillSpec.scala 16:13]
    _T_33[8] <= UInt<4>("h08") @[VecFillSpec.scala 16:13]
    _T_33[9] <= UInt<4>("h09") @[VecFillSpec.scala 16:13]
    _T_33[10] <= UInt<4>("h0a") @[VecFillSpec.scala 16:13]
    vec[0] <= _T_33[0] @[VecFillSpec.scala 16:7]
    vec[1] <= _T_33[1] @[VecFillSpec.scala 16:7]
    vec[2] <= _T_33[2] @[VecFillSpec.scala 16:7]
    vec[3] <= _T_33[3] @[VecFillSpec.scala 16:7]
    vec[4] <= _T_33[4] @[VecFillSpec.scala 16:7]
    vec[5] <= _T_33[5] @[VecFillSpec.scala 16:7]
    vec[6] <= _T_33[6] @[VecFillSpec.scala 16:7]
    vec[7] <= _T_33[7] @[VecFillSpec.scala 16:7]
    vec[8] <= _T_33[8] @[VecFillSpec.scala 16:7]
    vec[9] <= _T_33[9] @[VecFillSpec.scala 16:7]
    vec[10] <= _T_33[10] @[VecFillSpec.scala 16:7]
    node _T_48 = bits(io.addr, 3, 0)
    io.value <= vec[_T_48] @[VecFillSpec.scala 18:12]
    
