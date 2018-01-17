;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-03-31 17:47:22.700, builtAtMillis: 1490982442700
circuit ExampleTopModuleWithBB : 
  extmodule BB : 
    output analog2 : Analog<3>
    output analog1 : Analog<3>
    output z : SInt<16>
    input c : SInt<14>
    
    defname = BB
    
    
  module ExampleTopModuleWithBB : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<15>, flip b : UInt<15>, flip c : SInt<14>, x : UInt<16>, y : UInt<16>, z : SInt<16>, analog1 : Analog<3>, analog2 : Analog<3>, v : UInt<5>[3]}
    
    io is invalid
    io is invalid
    inst bb of BB @[AddIOPadsSpec.scala 68:18]
    bb.analog2 is invalid
    bb.analog1 is invalid
    bb.z is invalid
    bb.c is invalid
    bb.c <= io.c @[AddIOPadsSpec.scala 69:11]
    io.z <= bb.z @[AddIOPadsSpec.scala 70:8]
    attach (bb.analog1, io.analog1) @[AddIOPadsSpec.scala 71:17]
    attach (bb.analog2, io.analog2) @[AddIOPadsSpec.scala 72:17]
    node _T_24 = add(io.a, UInt<1>("h01")) @[AddIOPadsSpec.scala 74:16]
    node _T_25 = tail(_T_24, 1) @[AddIOPadsSpec.scala 74:16]
    io.x <= _T_25 @[AddIOPadsSpec.scala 74:8]
    node _T_27 = sub(io.b, UInt<1>("h01")) @[AddIOPadsSpec.scala 75:16]
    node _T_28 = asUInt(_T_27) @[AddIOPadsSpec.scala 75:16]
    node _T_29 = tail(_T_28, 1) @[AddIOPadsSpec.scala 75:16]
    io.y <= _T_29 @[AddIOPadsSpec.scala 75:8]
    io.v[0] <= io.a @[AddIOPadsSpec.scala 77:29]
    io.v[1] <= io.a @[AddIOPadsSpec.scala 77:29]
    io.v[2] <= io.a @[AddIOPadsSpec.scala 77:29]
    
