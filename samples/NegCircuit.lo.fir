circuit NegCircuit : @[:@2.0]
  extmodule BBFGreaterThan : @[:@3.2]
    output out : UInt<1> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFGreaterThan
    

  module NegCircuit : @[:@11.2]
    input clock : Clock @[:@12.4]
    input reset : UInt<1> @[:@13.4]
    input io_in1_node : UInt<64> @[:@14.4]
    input io_in2_node : UInt<64> @[:@14.4]
    output io_out : UInt<1> @[:@14.4]
  
    inst BBFGreaterThan of BBFGreaterThan @[DspReal.scala 59:32:@19.4]
    wire _T_11 : UInt<1> @[DspReal.scala 37:19:@25.4]
    io_out <= _T_11
    BBFGreaterThan.in2 <= io_in2_node
    BBFGreaterThan.in1 <= io_in1_node
    _T_11 <= BBFGreaterThan.out
