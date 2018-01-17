circuit OrderModule : @[:@2.0]
  extmodule BBFLessThan : @[:@3.2]
    output out : UInt<1> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFLessThan
    

  module OrderModule : @[:@11.2]
    input clock : Clock @[:@12.4]
    input reset : UInt<1> @[:@13.4]
    input io_in_node : UInt<64> @[:@14.4]
    output io_out_node : UInt<64> @[:@14.4]
  
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32:@19.4]
    wire _T_10 : UInt<1> @[DspReal.scala 37:19:@25.4]
    node _T_11_node = mux(_T_10, io_in_node, io_in_node) @[Order.scala 55:31:@28.4]
    io_out_node <= _T_11_node
    BBFLessThan.in2 <= io_in_node
    BBFLessThan.in1 <= io_in_node
    _T_10 <= BBFLessThan.out
