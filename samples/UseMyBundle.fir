;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit UseMyBundle : 
  module UseMyBundle : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip trigger : UInt<2>, outB : {x : UInt<8>, y : UInt<8>}, outC : {x : UInt<8>, y : UInt<8>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire _T_6 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 26:20]
    _T_6 is invalid @[BundleInitSpec.scala 26:20]
    _T_6.x <= UInt<4>("h08") @[BundleInitSpec.scala 27:12]
    _T_6.y <= UInt<4>("h09") @[BundleInitSpec.scala 28:12]
    reg regB : {x : UInt<8>, y : UInt<8>}, clock with : (reset => (reset, _T_6)) @[BundleInitSpec.scala 63:21]
    wire _T_11 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 26:20]
    _T_11 is invalid @[BundleInitSpec.scala 26:20]
    _T_11.x <= UInt<4>("h08") @[BundleInitSpec.scala 27:12]
    _T_11.y <= UInt<4>("h09") @[BundleInitSpec.scala 28:12]
    reg regC : {x : UInt<8>, y : UInt<8>}, clock with : (reset => (reset, _T_11)) @[BundleInitSpec.scala 64:21]
    node _T_16 = eq(io.trigger, UInt<1>("h01")) @[BundleInitSpec.scala 66:20]
    when _T_16 : @[BundleInitSpec.scala 66:29]
      wire _T_18 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 37:20]
      _T_18 is invalid @[BundleInitSpec.scala 37:20]
      _T_18.y <= UInt<3>("h05") @[BundleInitSpec.scala 38:12]
      regB.y <= _T_18.y @[BundleInitSpec.scala 67:10]
      regB.x <= _T_18.x @[BundleInitSpec.scala 67:10]
      wire _T_21 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 48:20]
      _T_21 is invalid @[BundleInitSpec.scala 48:20]
      _T_21.y <= regC.y @[BundleInitSpec.scala 49:10]
      _T_21.x <= regC.x @[BundleInitSpec.scala 49:10]
      _T_21.x <= UInt<3>("h07") @[BundleInitSpec.scala 50:12]
      regC.y <= _T_21.y @[BundleInitSpec.scala 68:10]
      regC.x <= _T_21.x @[BundleInitSpec.scala 68:10]
      skip @[BundleInitSpec.scala 66:29]
    node _T_24 = eq(io.trigger, UInt<2>("h02")) @[BundleInitSpec.scala 69:25]
    node _T_26 = eq(_T_16, UInt<1>("h00")) @[BundleInitSpec.scala 66:29]
    node _T_27 = and(_T_26, _T_24) @[BundleInitSpec.scala 69:34]
    when _T_27 : @[BundleInitSpec.scala 69:34]
      wire _T_29 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 48:20]
      _T_29 is invalid @[BundleInitSpec.scala 48:20]
      _T_29.y <= regB.y @[BundleInitSpec.scala 49:10]
      _T_29.x <= regB.x @[BundleInitSpec.scala 49:10]
      _T_29.x <= UInt<3>("h06") @[BundleInitSpec.scala 50:12]
      regB.y <= _T_29.y @[BundleInitSpec.scala 70:10]
      regB.x <= _T_29.x @[BundleInitSpec.scala 70:10]
      wire _T_32 : {x : UInt<8>, y : UInt<8>} @[BundleInitSpec.scala 37:20]
      _T_32 is invalid @[BundleInitSpec.scala 37:20]
      _T_32.y <= UInt<3>("h05") @[BundleInitSpec.scala 38:12]
      regC.y <= _T_32.y @[BundleInitSpec.scala 71:10]
      regC.x <= _T_32.x @[BundleInitSpec.scala 71:10]
      skip @[BundleInitSpec.scala 69:34]
    io.outB.y <= regB.y @[BundleInitSpec.scala 74:11]
    io.outB.x <= regB.x @[BundleInitSpec.scala 74:11]
    io.outC.y <= regC.y @[BundleInitSpec.scala 75:11]
    io.outC.x <= regC.x @[BundleInitSpec.scala 75:11]
    
