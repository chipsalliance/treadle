circuit RealAdder :
  extmodule BBFAdd :
    output out : UInt<64>
    input in2 : UInt<64>
    input in1 : UInt<64>


  module RealAdder :
    input clk : Clock
    input reset : UInt<1>
    output io : {flip a1 : {node : UInt<64>}, flip a2 : {node : UInt<64>}, c : {node : UInt<64>}}

    io is invalid
    reg register1 : {node : UInt<64>}, clk
    inst BBFAdd_1 of BBFAdd @[DspReal.scala 82:36]
    BBFAdd_1.out is invalid
    BBFAdd_1.in2 is invalid
    BBFAdd_1.in1 is invalid
    BBFAdd_1.in1 <= io.a1.node @[DspReal.scala 65:21]
    BBFAdd_1.in2 <= io.a2.node @[DspReal.scala 66:21]
    wire T_15 : {node : UInt<64>} @[DspReal.scala 67:19]
    T_15 is invalid @[DspReal.scala 67:19]
    T_15.node <= BBFAdd_1.out @[DspReal.scala 68:14]
    register1 <- T_15 @[RealAdderSpec.scala 19:13]
    io.c <- register1 @[RealAdderSpec.scala 21:8]