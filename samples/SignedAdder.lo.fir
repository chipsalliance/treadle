circuit SignedAdder : @[:@2.0]
  module SignedAdder : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in0 : SInt<16> @[:@6.4]
    input io_in1 : SInt<16> @[:@6.4]
    output io_out : SInt<16> @[:@6.4]
  
    node _T_5 = add(io_in0, io_in1) @[Adder.scala 89:20:@11.4]
    node _T_6 = tail(_T_5, 1) @[Adder.scala 89:20:@12.4]
    node _T_7 = asSInt(_T_6) @[Adder.scala 89:20:@13.4]
    io_out <= _T_7
