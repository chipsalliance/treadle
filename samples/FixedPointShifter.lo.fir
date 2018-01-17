circuit FixedPointShifter : @[:@2.0]
  module FixedPointShifter : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_inValue : SInt<8> @[:@6.4]
    input io_dynamicShiftValue : UInt<3> @[:@6.4]
    output io_shiftLeftResult : SInt<17> @[:@6.4]
    output io_dynamicShiftRightResult : SInt<8> @[:@6.4]
    output io_dynamicShiftLeftResult : SInt<15> @[:@6.4]
  
    node _T_7 = shl(io_inValue, 9) @[FixedPointSpec.scala 66:36:@11.4]
    node _T_8 = dshl(io_inValue, io_dynamicShiftValue) @[FixedPointSpec.scala 70:43:@13.4]
    node _T_9 = dshr(io_inValue, io_dynamicShiftValue) @[FixedPointSpec.scala 71:44:@15.4]
    io_shiftLeftResult <= _T_7
    io_dynamicShiftRightResult <= _T_9
    io_dynamicShiftLeftResult <= _T_8
