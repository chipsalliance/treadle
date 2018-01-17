circuit SimpleCaseClassModule : @[:@2.0]
  module SimpleCaseClassModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in_underlying : SInt<5> @[:@6.4]
    output io_out_underlying : SInt<5> @[:@6.4]
  
    reg register1_underlying : SInt<5>, clock with :
      reset => (UInt<1>("h0"), register1_underlying) @[CaseClassBundleSpec.scala 27:22:@11.4]
    io_out_underlying <= register1_underlying
    register1_underlying <= io_in_underlying
