circuit BundlePassThrough : @[:@2.0]
  module BundlePassThrough : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_inBundle_u1 : UInt<3> @[:@6.4]
    input io_inBundle_u2 : UInt<9> @[:@6.4]
    input io_inBundle_u3 : UInt<27> @[:@6.4]
    output io_outBundle_u1 : UInt<3> @[:@6.4]
    output io_outBundle_u2 : UInt<9> @[:@6.4]
    output io_outBundle_u3 : UInt<27> @[:@6.4]
    output io_outBundleAsUInt : UInt<9> @[:@6.4]
  
    reg regBundle_u1 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), regBundle_u1) @[AggregateOrderingSpec.scala 40:22:@11.4]
    reg regBundle_u2 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), regBundle_u2) @[AggregateOrderingSpec.scala 40:22:@11.4]
    reg regBundle_u3 : UInt<27>, clock with :
      reset => (UInt<1>("h0"), regBundle_u3) @[AggregateOrderingSpec.scala 40:22:@11.4]
    node _T_6 = cat(io_inBundle_u1, io_inBundle_u2) @[AggregateOrderingSpec.scala 45:43:@18.4]
    node _T_7 = cat(_T_6, io_inBundle_u3) @[AggregateOrderingSpec.scala 45:43:@19.4]
    io_outBundle_u1 <= regBundle_u1
    io_outBundle_u2 <= regBundle_u2
    io_outBundle_u3 <= regBundle_u3
    io_outBundleAsUInt <= bits(_T_7, 8, 0)
    regBundle_u1 <= io_inBundle_u1
    regBundle_u2 <= io_inBundle_u2
    regBundle_u3 <= io_inBundle_u3
