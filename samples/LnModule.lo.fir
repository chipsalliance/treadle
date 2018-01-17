circuit LnModule : @[:@2.0]
  extmodule BBFLn : @[:@3.2]
    output out : UInt<64> @[:@4.4]
    input in : UInt<64> @[:@5.4]
  
    defname = BBFLn
    

  module LnModule : @[:@10.2]
    input clock : Clock @[:@11.4]
    input reset : UInt<1> @[:@12.4]
    input io_num_node : UInt<64> @[:@13.4]
    output io_ln_node : UInt<64> @[:@13.4]
  
    inst BBFLn of BBFLn @[DspReal.scala 83:30:@18.4]
    wire _T_8_node : UInt<64> @[DspReal.scala 19:19:@22.4]
    io_ln_node <= _T_8_node
    BBFLn.in <= io_num_node
    _T_8_node <= BBFLn.out
