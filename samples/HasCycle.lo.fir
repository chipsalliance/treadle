circuit HasCycle : @[:@2.0]
  module HasCycle : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<1> @[:@6.4]
    output io_o : UInt<1> @[:@6.4]
  
    wire b : UInt<1> @[CycleTest.scala 16:15:@11.4]
    node _T_5 = and(b, io_a) @[CycleTest.scala 17:9:@13.4]
    io_o <= b
    b <= _T_5
