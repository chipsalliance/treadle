circuit ContextNestingTop : @[:@2.0]
  module ContextNestingBottom : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in1 : UInt<4> @[:@6.4]
    input io_in2 : UInt<4> @[:@6.4]
    output io_outDefault : UInt<5> @[:@6.4]
    output io_outWrap : UInt<5> @[:@6.4]
    output io_outGrow : UInt<5> @[:@6.4]
  
    node _T_2 = add(io_in1, io_in2) @[UIntTypeClass.scala 23:22:@11.4]
    node _T_3 = tail(_T_2, 1) @[UIntTypeClass.scala 23:22:@12.4]
    node _T_5 = add(io_in1, io_in2) @[UIntTypeClass.scala 22:22:@14.4]
    node _T_7 = add(io_in1, io_in2) @[UIntTypeClass.scala 23:22:@16.4]
    node _T_8 = tail(_T_7, 1) @[UIntTypeClass.scala 23:22:@17.4]
    io_outDefault <= _T_8
    io_outWrap <= _T_3
    io_outGrow <= _T_5

  module ContextNestingBottom_1 : @[:@20.2]
    input clock : Clock @[:@21.4]
    input reset : UInt<1> @[:@22.4]
    input io_in1 : UInt<4> @[:@23.4]
    input io_in2 : UInt<4> @[:@23.4]
    output io_outDefault : UInt<5> @[:@23.4]
    output io_outWrap : UInt<5> @[:@23.4]
    output io_outGrow : UInt<5> @[:@23.4]
  
    node _T_2 = add(io_in1, io_in2) @[UIntTypeClass.scala 23:22:@28.4]
    node _T_3 = tail(_T_2, 1) @[UIntTypeClass.scala 23:22:@29.4]
    node _T_5 = add(io_in1, io_in2) @[UIntTypeClass.scala 22:22:@31.4]
    node _T_7 = add(io_in1, io_in2) @[UIntTypeClass.scala 22:22:@33.4]
    io_outDefault <= _T_7
    io_outWrap <= _T_3
    io_outGrow <= _T_5

  module ContextNestingTop : @[:@36.2]
    input clock : Clock @[:@37.4]
    input reset : UInt<1> @[:@38.4]
    input io_in1 : UInt<4> @[:@39.4]
    input io_in2 : UInt<4> @[:@39.4]
    output io_mod1Default : UInt<5> @[:@39.4]
    output io_mod1Wrap : UInt<5> @[:@39.4]
    output io_mod1Grow : UInt<5> @[:@39.4]
    output io_mod2Default : UInt<5> @[:@39.4]
    output io_mod2Wrap : UInt<5> @[:@39.4]
    output io_mod2Grow : UInt<5> @[:@39.4]
  
    inst ContextNestingBottom of ContextNestingBottom @[DspContextSpec.scala 107:64:@44.4]
    inst ContextNestingBottom_1 of ContextNestingBottom_1 @[DspContextSpec.scala 108:64:@48.4]
    io_mod1Default <= ContextNestingBottom.io_outDefault
    io_mod1Wrap <= ContextNestingBottom.io_outWrap
    io_mod1Grow <= ContextNestingBottom.io_outGrow
    io_mod2Default <= ContextNestingBottom_1.io_outDefault
    io_mod2Wrap <= ContextNestingBottom_1.io_outWrap
    io_mod2Grow <= ContextNestingBottom_1.io_outGrow
    ContextNestingBottom.io_in1 <= io_in1
    ContextNestingBottom.io_in2 <= io_in2
    ContextNestingBottom.clock <= clock
    ContextNestingBottom.reset <= reset
    ContextNestingBottom_1.io_in1 <= io_in1
    ContextNestingBottom_1.io_in2 <= io_in2
    ContextNestingBottom_1.clock <= clock
    ContextNestingBottom_1.reset <= reset
