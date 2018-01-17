circuit OverflowTypeCircuit : @[:@2.0]
  module OverflowTypeCircuit : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : SInt<4> @[:@6.4]
    input io_b : SInt<4> @[:@6.4]
    output io_addWrap : SInt<5> @[:@6.4]
    output io_addGrow : SInt<5> @[:@6.4]
    output io_subWrap : SInt<5> @[:@6.4]
    output io_subGrow : SInt<5> @[:@6.4]
  
    node _T_2 = add(io_a, io_b) @[SIntTypeClass.scala 23:22:@11.4]
    node _T_3 = tail(_T_2, 1) @[SIntTypeClass.scala 23:22:@12.4]
    node _T_4 = asSInt(_T_3) @[SIntTypeClass.scala 23:22:@13.4]
    reg regAddWrap : SInt<4>, clock with :
      reset => (UInt<1>("h0"), regAddWrap) @[NumbersSpec.scala 194:27:@14.4]
    node _T_7 = add(io_a, io_b) @[SIntTypeClass.scala 22:22:@16.4]
    reg regAddGrow : SInt<5>, clock with :
      reset => (UInt<1>("h0"), regAddGrow) @[NumbersSpec.scala 195:27:@17.4]
    node _T_10 = sub(io_a, io_b) @[SIntTypeClass.scala 32:22:@19.4]
    node _T_11 = tail(_T_10, 1) @[SIntTypeClass.scala 32:22:@20.4]
    node _T_12 = asSInt(_T_11) @[SIntTypeClass.scala 32:22:@21.4]
    reg regSubWrap : SInt<4>, clock with :
      reset => (UInt<1>("h0"), regSubWrap) @[NumbersSpec.scala 197:27:@22.4]
    node _T_15 = sub(io_a, io_b) @[SIntTypeClass.scala 31:22:@24.4]
    reg regSubGrow : SInt<5>, clock with :
      reset => (UInt<1>("h0"), regSubGrow) @[NumbersSpec.scala 198:27:@25.4]
    io_addWrap <= regAddWrap
    io_addGrow <= regAddGrow
    io_subWrap <= regSubWrap
    io_subGrow <= regSubGrow
    regAddWrap <= _T_4
    regAddGrow <= _T_7
    regSubWrap <= _T_12
    regSubGrow <= _T_15
