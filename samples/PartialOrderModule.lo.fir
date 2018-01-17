circuit PartialOrderModule : @[:@2.0]
  extmodule BBFEquals : @[:@3.2]
    output out : UInt<1> @[:@4.4]
    input in2 : UInt<64> @[:@5.4]
    input in1 : UInt<64> @[:@6.4]
  
    defname = BBFEquals
    

  extmodule BBFLessThan : @[:@11.2]
    output out : UInt<1> @[:@12.4]
    input in2 : UInt<64> @[:@13.4]
    input in1 : UInt<64> @[:@14.4]
  
    defname = BBFLessThan
    

  module PartialOrderModule : @[:@19.2]
    input clock : Clock @[:@20.4]
    input reset : UInt<1> @[:@21.4]
    input io_in_node : UInt<64> @[:@22.4]
    output io_out_node : UInt<64> @[:@22.4]
  
    inst BBFEquals of BBFEquals @[DspReal.scala 75:32:@27.4]
    wire _T_10 : UInt<1> @[DspReal.scala 37:19:@33.4]
    inst BBFLessThan of BBFLessThan @[DspReal.scala 67:32:@36.4]
    wire _T_13 : UInt<1> @[DspReal.scala 37:19:@42.4]
    wire _T_16_eq : UInt<1> @[Comparison.scala 25:19:@45.4]
    wire _T_16_lt : UInt<1> @[Comparison.scala 25:19:@45.4]
    wire _T_23_valid : UInt<1> @[Comparison.scala 18:19:@49.4]
    wire _T_23_bits_eq : UInt<1> @[Comparison.scala 18:19:@49.4]
    wire _T_23_bits_lt : UInt<1> @[Comparison.scala 18:19:@49.4]
    node _T_26_node = mux(_T_23_bits_eq, io_in_node, io_in_node) @[TypeclassSpec.scala 56:8:@54.4]
    io_out_node <= _T_26_node
    BBFEquals.in2 <= io_in_node
    BBFEquals.in1 <= io_in_node
    _T_10 <= BBFEquals.out
    BBFLessThan.in2 <= io_in_node
    BBFLessThan.in1 <= io_in_node
    _T_13 <= BBFLessThan.out
    _T_16_eq <= _T_10
    _T_16_lt <= _T_13
    _T_23_valid <= UInt<1>("h1")
    _T_23_bits_eq <= _T_16_eq
    _T_23_bits_lt <= _T_16_lt
