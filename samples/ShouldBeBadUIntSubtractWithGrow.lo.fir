circuit ShouldBeBadUIntSubtractWithGrow : @[:@2.0]
  module ShouldBeBadUIntSubtractWithGrow : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<4> @[:@6.4]
    input io_b : UInt<4> @[:@6.4]
    output io_o : UInt<4> @[:@6.4]
  
    node _T_5 = sub(io_a, io_b) @[NumbersSpec.scala 215:60:@11.4]
    node _T_6 = asUInt(_T_5) @[NumbersSpec.scala 215:60:@12.4]
    node _T_7 = tail(_T_6, 1) @[NumbersSpec.scala 215:60:@13.4]
    reg r : UInt<4>, clock with :
      reset => (UInt<1>("h0"), r) @[NumbersSpec.scala 215:18:@14.4]
    io_o <= r
    r <= _T_7
