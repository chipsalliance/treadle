circuit AccumBlackBoxWrapper : @[:@2.0]
  extmodule AccumBlackBox : @[:@3.2]
    input clock : Clock @[:@4.4]
    output data : UInt<16> @[:@5.4]
  
    defname = AccumBlackBox
    

  module AccumBlackBoxWrapper : @[:@10.2]
    input clock : Clock @[:@11.4]
    input reset : UInt<1> @[:@12.4]
    output io_data : UInt<16> @[:@13.4]
    input io_clock : Clock @[:@13.4]
  
    inst m of AccumBlackBox @[AccumBlackBoxSpec.scala 93:17:@18.4]
    node _T_4 = bits(reset, 0, 0) @[AccumBlackBoxSpec.scala 96:9:@23.4]
    node _T_6 = eq(_T_4, UInt<1>("h0")) @[AccumBlackBoxSpec.scala 96:9:@24.4]
    io_data <= m.data
    m.clock <= clock
    printf(clock, and(and(UInt<1>("h1"), _T_6), UInt<1>("h1")), "m.io.data %d io.data %d\n", m.data, io_data) @[AccumBlackBoxSpec.scala 96:9:@26.6]
