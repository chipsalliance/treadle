;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-07-19 19:29:28.307, builtAtMillis: 1500492568307
circuit TopOfVisualizer : 
  module VizModC : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<16>, out : UInt<16>}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.out <= io.in @[VisualizerSpec.scala 25:10]
    
  module VizModA : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt, out : UInt}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst modC of VizModC @[VisualizerSpec.scala 40:20]
    modC.io is invalid
    modC.clock <= clock
    modC.reset <= reset
    modC.io.in <= io.in @[VisualizerSpec.scala 41:14]
    io.out <= modC.io.out @[VisualizerSpec.scala 42:10]
    
  module TopOfVisualizer : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : UInt<32>, flip in2 : UInt<32>, flip select : UInt<1>, out : UInt<32>, memOut : UInt<32>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<32>, clock @[VisualizerSpec.scala 65:14]
    reg y : UInt<32>, clock @[VisualizerSpec.scala 66:14]
    cmem myMem : UInt<32>[16] @[VisualizerSpec.scala 68:18]
    inst modA of VizModA @[VisualizerSpec.scala 70:20]
    modA.io is invalid
    modA.clock <= clock
    modA.reset <= reset
    when io.select : @[VisualizerSpec.scala 75:19]
      x <= io.in1 @[VisualizerSpec.scala 76:7]
      node _T_11 = bits(io.in1, 3, 0)
      infer mport _T_12 = myMem[_T_11], clock
      _T_12 <= io.in2 @[VisualizerSpec.scala 77:19]
      skip @[VisualizerSpec.scala 75:19]
    node _T_14 = eq(io.select, UInt<1>("h00")) @[VisualizerSpec.scala 75:19]
    when _T_14 : @[VisualizerSpec.scala 79:14]
      x <= io.in2 @[VisualizerSpec.scala 80:7]
      node _T_15 = bits(io.in1, 3, 0)
      infer mport _T_16 = myMem[_T_15], clock
      io.memOut <= _T_16 @[VisualizerSpec.scala 81:15]
      skip @[VisualizerSpec.scala 79:14]
    modA.io.in <= x @[VisualizerSpec.scala 84:14]
    node _T_17 = add(modA.io.out, io.in2) @[VisualizerSpec.scala 86:20]
    node _T_18 = tail(_T_17, 1) @[VisualizerSpec.scala 86:20]
    y <= _T_18 @[VisualizerSpec.scala 86:5]
    io.out <= y @[VisualizerSpec.scala 87:10]
    
