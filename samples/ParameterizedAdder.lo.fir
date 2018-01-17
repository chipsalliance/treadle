circuit ParameterizedAdder : @[:@2.0]
  extmodule BBFAdd : @[:@3.2]
    output out : UInt<64> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFAdd
    

  module ParameterizedAdder : @[:@11.2]
    input clock : Clock @[:@12.4]
    input reset : UInt<1> @[:@13.4]
    input io_a1_node : UInt<64> @[:@14.4]
    input io_a2_node : UInt<64> @[:@14.4]
    output io_c_node : UInt<64> @[:@14.4]
  
    reg register1_node : UInt<64>, clock with :
      reset => (UInt<1>("h0"), register1_node) @[ParameterizedAdderSpec.scala 20:22:@19.4]
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36:@20.4]
    wire _T_24_node : UInt<64> @[DspReal.scala 28:19:@26.4]
    io_c_node <= register1_node
    register1_node <= _T_24_node
    BBFAdd.in2 <= io_a2_node
    BBFAdd.in1 <= io_a1_node
    _T_24_node <= BBFAdd.out
