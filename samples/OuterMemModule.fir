;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-25 20:31:59.290, builtAtMillis: 1506371519290
circuit OuterMemModule : 
  module InnerMemModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {}
    
    clock is invalid
    reset is invalid
    io is invalid
    cmem nelly : UInt<32>[1024] @[MemPokeSpec.scala 13:18]
    
  module OuterMemModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip readAddress : UInt<10>, readData : UInt<32>}
    
    clock is invalid
    reset is invalid
    io is invalid
    cmem billy : UInt<32>[1024] @[MemPokeSpec.scala 22:18]
    inst inner of InnerMemModule @[MemPokeSpec.scala 23:21]
    inner.io is invalid
    inner.clock <= clock
    inner.reset <= reset
    infer mport _T_6 = billy[io.readAddress], clock
    io.readData <= _T_6 @[MemPokeSpec.scala 25:15]
    
