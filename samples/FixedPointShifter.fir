;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit FixedPointShifter : 
  module FixedPointShifter : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inValue : Fixed<8><<9>>, flip dynamicShiftValue : UInt<3>, shiftLeftResult : Fixed<17><<9>>, dynamicShiftRightResult : Fixed<8><<9>>, dynamicShiftLeftResult : Fixed<15><<9>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_7 = shl(io.inValue, 9) @[FixedPointSpec.scala 66:36]
    io.shiftLeftResult <= _T_7 @[FixedPointSpec.scala 66:22]
    node _T_8 = dshl(io.inValue, io.dynamicShiftValue) @[FixedPointSpec.scala 70:43]
    io.dynamicShiftLeftResult <= _T_8 @[FixedPointSpec.scala 70:29]
    node _T_9 = dshr(io.inValue, io.dynamicShiftValue) @[FixedPointSpec.scala 71:44]
    io.dynamicShiftRightResult <= _T_9 @[FixedPointSpec.scala 71:30]
    
