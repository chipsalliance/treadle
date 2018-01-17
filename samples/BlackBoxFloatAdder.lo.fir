circuit BlackBoxFloatAdder : @[:@2.0]
  extmodule BBFAdd : @[:@3.2]
    output out : UInt<64> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFAdd
    

  module BlackBoxFloatAdder : @[:@27.2]
    input clock : Clock @[:@28.4]
    input reset : UInt<1> @[:@29.4]
    input io_a_node : UInt<64> @[:@30.4]
    input io_b_node : UInt<64> @[:@30.4]
    output io_c_node : UInt<64> @[:@30.4]
    output io_d_node : UInt<64> @[:@30.4]
    output io_e_node : UInt<64> @[:@30.4]
  
    inst BBFAdd of BBFAdd @[DspReal.scala 43:36:@35.4]
    wire _T_14_node : UInt<64> @[DspReal.scala 28:19:@41.4]
    inst BBFAdd_1 of BBFAdd @[DspReal.scala 43:36:@45.4]
    wire _T_18_node : UInt<64> @[DspReal.scala 28:19:@51.4]
    inst BBFAdd_2 of BBFAdd @[DspReal.scala 43:36:@55.4]
    wire _T_22_node : UInt<64> @[DspReal.scala 28:19:@61.4]
    io_c_node <= _T_14_node
    io_d_node <= _T_18_node
    io_e_node <= _T_22_node
    BBFAdd.in2 <= io_b_node
    BBFAdd.in1 <= io_a_node
    _T_14_node <= BBFAdd.out
    BBFAdd_1.in2 <= io_a_node
    BBFAdd_1.in1 <= io_a_node
    _T_18_node <= BBFAdd_1.out
    BBFAdd_2.in2 <= io_b_node
    BBFAdd_2.in1 <= io_b_node
    _T_22_node <= BBFAdd_2.out
