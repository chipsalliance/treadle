circuit IntegerModule : @[:@2.0]
  module IntegerModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<10> @[:@6.4]
    output io_out : SInt<10> @[:@6.4]
  
    node _T_4 = rem(io_in, io_in) @[SIntTypeClass.scala 86:39:@11.4]
    node _T_5 = add(io_in, _T_4) @[SIntTypeClass.scala 18:40:@12.4]
    node _T_6 = tail(_T_5, 1) @[SIntTypeClass.scala 18:40:@13.4]
    node _T_7 = asSInt(_T_6) @[SIntTypeClass.scala 18:40:@14.4]
    io_out <= _T_7
