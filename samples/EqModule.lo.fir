circuit EqModule : @[:@2.0]
  module EqModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : SInt<10> @[:@6.4]
    output io_out : SInt<10> @[:@6.4]
  
    node _T_5 = eq(io_in, asSInt(UInt<1>("h0"))) @[SIntTypeClass.scala 55:48:@11.4]
    node _T_7 = mux(_T_5, asSInt(UInt<2>("h1")), io_in) @[TypeclassSpec.scala 38:51:@12.4]
    io_out <= _T_7
