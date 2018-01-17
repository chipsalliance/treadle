circuit A : @[:@2.0]
  module B : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_enable : UInt<1> @[:@6.4]
    output io_calc : UInt<3> @[:@6.4]
  
    reg counter : UInt<3>, clock with :
      reset => (reset, UInt<3>("h0")) @[ATester.scala 27:24:@11.4]
    node _T_7 = add(counter, UInt<1>("h1")) @[ATester.scala 29:24:@13.6]
    node _T_8 = tail(_T_7, 1) @[ATester.scala 29:24:@14.6]
    node _GEN_0 = mux(io_enable, _T_8, counter) @[ATester.scala 28:19:@12.4]
    io_calc <= counter
    counter <= _GEN_0

  module A : @[:@19.2]
    input clock : Clock @[:@20.4]
    input reset : UInt<1> @[:@21.4]
    input io_enable : UInt<1> @[:@22.4]
    output io_calc : UInt<3> @[:@22.4]
  
    inst sub of B @[ATester.scala 17:19:@27.4]
    io_calc <= sub.io_calc
    sub.io_enable <= io_enable
    sub.clock <= clock
    sub.reset <= reset
