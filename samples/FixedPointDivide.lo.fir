circuit FixedPointDivide : @[:@2.0]
  module FixedPointDivide : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<10> @[:@6.4]
    output io_out : SInt<10> @[:@6.4]
  
    node _T_2 = asUInt(io_in) @[FixedPointSpec.scala 39:20:@11.4]
    node _T_3 = shr(_T_2, 2) @[FixedPointSpec.scala 39:27:@12.4]
    node _T_4 = asSInt(_T_3) @[FixedPointSpec.scala 39:55:@13.4]
    io_out <= _T_4
