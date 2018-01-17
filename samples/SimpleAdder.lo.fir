circuit SimpleAdder : @[:@2.0]
  module SimpleAdder : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a1 : SInt<6> @[:@6.4]
    input io_a2 : SInt<8> @[:@6.4]
    output io_c : SInt<12> @[:@6.4]
  
    reg register1 : SInt<11>, clock with :
      reset => (UInt<1>("h0"), register1) @[SimpleAdderSpec.scala 19:22:@11.4]
    node _T_6 = add(io_a1, shl(io_a2, 3)) @[SimpleAdderSpec.scala 21:22:@12.4]
    node _T_7 = tail(_T_6, 1) @[SimpleAdderSpec.scala 21:22:@13.4]
    node _T_8 = asSInt(_T_7) @[SimpleAdderSpec.scala 21:22:@14.4]
    io_c <= shl(register1, 1)
    register1 <= _T_8
