;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit UsesBBAddOne : 
  extmodule BBAddOne : 
    output out : UInt<16>
    input in : UInt<16>
    
    defname = BBAddOne
    
    
  extmodule BBAddTwo : 
    output out : UInt<16>
    input in : UInt<16>
    
    defname = BBAddTwo
    
    
  extmodule BBAddThree : 
    output out : UInt<16>
    input in : UInt<16>
    
    defname = BBAddThree
    
    
  module UsesBBAddOne : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<16>, out1 : UInt<16>, out2 : UInt<16>, out3 : UInt<16>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst bbAddOne of BBAddOne @[BlackBoxVerilogDeliverySpec.scala 50:24]
    bbAddOne.out is invalid
    bbAddOne.in is invalid
    bbAddOne.in <= io.in @[BlackBoxVerilogDeliverySpec.scala 51:18]
    io.out1 <= bbAddOne.out @[BlackBoxVerilogDeliverySpec.scala 52:11]
    inst bbAddTwo of BBAddTwo @[BlackBoxVerilogDeliverySpec.scala 54:24]
    bbAddTwo.out is invalid
    bbAddTwo.in is invalid
    bbAddTwo.in <= io.in @[BlackBoxVerilogDeliverySpec.scala 55:18]
    io.out2 <= bbAddTwo.out @[BlackBoxVerilogDeliverySpec.scala 56:11]
    inst bbAddThree of BBAddThree @[BlackBoxVerilogDeliverySpec.scala 58:26]
    bbAddThree.out is invalid
    bbAddThree.in is invalid
    bbAddThree.in <= io.in @[BlackBoxVerilogDeliverySpec.scala 59:20]
    io.out3 <= bbAddThree.out @[BlackBoxVerilogDeliverySpec.scala 60:11]
    
