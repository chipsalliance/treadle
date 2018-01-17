;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit AccumBlackBoxWrapper : 
  extmodule AccumBlackBox : 
    input clock : Clock
    output data : UInt<16>
    
    defname = AccumBlackBox
    
    
  module AccumBlackBoxWrapper : 
    input clock : Clock
    input reset : UInt<1>
    output io : {data : UInt<16>, flip clock : Clock}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst m of AccumBlackBox @[AccumBlackBoxSpec.scala 93:17]
    m.clock is invalid
    m.data is invalid
    io.data <= m.data @[AccumBlackBoxSpec.scala 94:11]
    m.clock <= clock @[AccumBlackBoxSpec.scala 95:14]
    node _T_5 = eq(reset, UInt<1>("h00")) @[AccumBlackBoxSpec.scala 96:9]
    when _T_5 : @[AccumBlackBoxSpec.scala 96:9]
      printf(clock, UInt<1>(1), "m.io.data %d io.data %d\n", m.data, io.data) @[AccumBlackBoxSpec.scala 96:9]
      skip @[AccumBlackBoxSpec.scala 96:9]
    
