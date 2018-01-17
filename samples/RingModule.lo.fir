circuit RingModule : @[:@2.0]
  module RingModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<10> @[:@6.4]
    output io_out : SInt<10> @[:@6.4]
  
    node _T_6 = add(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 18:40:@11.4]
    node _T_7 = tail(_T_6, 1) @[SIntTypeClass.scala 18:40:@12.4]
    node _T_8 = asSInt(_T_7) @[SIntTypeClass.scala 18:40:@13.4]
    node _T_9 = mul(asSInt(UInt<2>("h1")), _T_8) @[SIntTypeClass.scala 44:41:@14.4]
    io_out <= asSInt(bits(_T_9, 9, 0))
