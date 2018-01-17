circuit BBWrapper : @[:@2.0]
  extmodule BB : @[:@3.2]
    output result : UInt<1> @[:@4.4]
    input b : UInt<1> @[:@5.4]
    input a : UInt<1> @[:@6.4]
  
    defname = BB
    

  module BBWrapper : @[:@11.2]
    input clock : Clock @[:@12.4]
    input reset : UInt<1> @[:@13.4]
    input io_a : UInt<1> @[:@14.4]
    input io_b : UInt<1> @[:@14.4]
    output io_result : UInt<1> @[:@14.4]
  
    inst tb of BB @[BlackBoxAnd.scala 29:18:@19.4]
    io_result <= tb.result
    tb.b <= io_b
    tb.a <= io_a
