;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-09 21:53:49.033, builtAtMillis: 1497045229033
circuit A : 
  module B : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip enable : UInt<1>, calc : UInt<3>}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg counter : UInt<3>, clock with : (reset => (reset, UInt<3>("h00"))) @[ATester.scala 27:24]
    when io.enable : @[ATester.scala 28:19]
      node _T_7 = add(counter, UInt<1>("h01")) @[ATester.scala 29:24]
      node _T_8 = tail(_T_7, 1) @[ATester.scala 29:24]
      counter <= _T_8 @[ATester.scala 29:13]
      skip @[ATester.scala 28:19]
    io.calc <= counter @[ATester.scala 31:11]
    
  module A : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip enable : UInt<1>, calc : UInt<3>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst sub of B @[ATester.scala 17:19]
    sub.io is invalid
    sub.clock <= clock
    sub.reset <= reset
    sub.io.enable <= io.enable @[ATester.scala 18:17]
    io.calc <= sub.io.calc @[ATester.scala 19:11]
    
