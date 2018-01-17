circuit ParameterizedNumberOperation : @[:@2.0]
  module ParameterizedNumberOperation : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a1 : SInt<16> @[:@6.4]
    input io_a2 : SInt<16> @[:@6.4]
    output io_c : SInt<48> @[:@6.4]
  
    reg register1 : SInt<48>, clock with :
      reset => (UInt<1>("h0"), register1) @[ParameterizedOpSpec.scala 27:22:@11.4]
    node _T_10 = mul(io_a1, io_a2) @[FixedPointTypeClass.scala 43:59:@12.4]
    io_c <= register1
    register1 <= shr(_T_10, 8)
