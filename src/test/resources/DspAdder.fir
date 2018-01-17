circuit RealAdder :
  extmodule BBFAdd :
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>


  module RealAdder :
    input clk : Clock
    input reset : UInt<1>
    input io_a1_node : UInt<64>
    input io_a2_node : UInt<64>
    output io_c_node : UInt<64>

    reg register1_node : UInt<64>, clk with :
      reset => (UInt<1>("h0"), register1_node)
    inst BBFAdd_1 of BBFAdd @[DspReal.scala 82:36]
    wire T_15_node : UInt<64> @[DspReal.scala 67:19]
    io_c_node <= register1_node
    register1_node <= T_15_node
    BBFAdd_1.in2 <= io_a2_node
    BBFAdd_1.in1 <= io_a1_node
    T_15_node <= BBFAdd_1.out
