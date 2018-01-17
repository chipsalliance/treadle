circuit UseMyBundle : @[:@2.0]
  module UseMyBundle : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_trigger : UInt<2> @[:@6.4]
    output io_outB_x : UInt<8> @[:@6.4]
    output io_outB_y : UInt<8> @[:@6.4]
    output io_outC_x : UInt<8> @[:@6.4]
    output io_outC_y : UInt<8> @[:@6.4]
  
    wire _T_6_x : UInt<8> @[BundleInitSpec.scala 26:20:@11.4]
    wire _T_6_y : UInt<8> @[BundleInitSpec.scala 26:20:@11.4]
    reg regB_x : UInt<8>, clock with :
      reset => (UInt<1>("h0"), regB_x) @[BundleInitSpec.scala 63:21:@15.4]
    reg regB_y : UInt<8>, clock with :
      reset => (UInt<1>("h0"), regB_y) @[BundleInitSpec.scala 63:21:@15.4]
    wire _T_11_x : UInt<8> @[BundleInitSpec.scala 26:20:@16.4]
    wire _T_11_y : UInt<8> @[BundleInitSpec.scala 26:20:@16.4]
    reg regC_x : UInt<8>, clock with :
      reset => (UInt<1>("h0"), regC_x) @[BundleInitSpec.scala 64:21:@20.4]
    reg regC_y : UInt<8>, clock with :
      reset => (UInt<1>("h0"), regC_y) @[BundleInitSpec.scala 64:21:@20.4]
    node _T_16 = eq(io_trigger, UInt<1>("h1")) @[BundleInitSpec.scala 66:20:@21.4]
    wire _T_18_x : UInt<8> @[BundleInitSpec.scala 37:20:@23.6]
    wire _T_18_y : UInt<8> @[BundleInitSpec.scala 37:20:@23.6]
    wire _T_21_x : UInt<8> @[BundleInitSpec.scala 48:20:@28.6]
    wire _T_21_y : UInt<8> @[BundleInitSpec.scala 48:20:@28.6]
    node _T_24 = eq(io_trigger, UInt<2>("h2")) @[BundleInitSpec.scala 69:25:@37.6]
    wire _T_26_x : UInt<8> @[BundleInitSpec.scala 48:20:@39.8]
    wire _T_26_y : UInt<8> @[BundleInitSpec.scala 48:20:@39.8]
    wire _T_29_x : UInt<8> @[BundleInitSpec.scala 37:20:@46.8]
    wire _T_29_y : UInt<8> @[BundleInitSpec.scala 37:20:@46.8]
    node _GEN_0 = mux(_T_24, _T_26_y, regB_y) @[BundleInitSpec.scala 69:34:@38.6]
    node _GEN_1 = mux(_T_24, _T_26_x, regB_x) @[BundleInitSpec.scala 69:34:@38.6]
    node _GEN_2 = mux(_T_24, _T_29_y, regC_y) @[BundleInitSpec.scala 69:34:@38.6]
    node _GEN_3 = mux(_T_24, _T_29_x, regC_x) @[BundleInitSpec.scala 69:34:@38.6]
    node _GEN_4 = mux(_T_16, _T_18_y, _GEN_0) @[BundleInitSpec.scala 66:29:@22.4]
    node _GEN_5 = mux(_T_16, _T_18_x, _GEN_1) @[BundleInitSpec.scala 66:29:@22.4]
    node _GEN_6 = mux(_T_16, _T_21_y, _GEN_2) @[BundleInitSpec.scala 66:29:@22.4]
    node _GEN_7 = mux(_T_16, _T_21_x, _GEN_3) @[BundleInitSpec.scala 66:29:@22.4]
    io_outB_x <= regB_x
    io_outB_y <= regB_y
    io_outC_x <= regC_x
    io_outC_y <= regC_y
    _T_6_x <= UInt<4>("h8")
    _T_6_y <= UInt<4>("h9")
    regB_x <= mux(reset, _T_6_x, _GEN_5)
    regB_y <= mux(reset, _T_6_y, _GEN_4)
    _T_11_x <= UInt<4>("h8")
    _T_11_y <= UInt<4>("h9")
    regC_x <= mux(reset, _T_11_x, _GEN_7)
    regC_y <= mux(reset, _T_11_y, _GEN_6)
    _T_18_x is invalid
    _T_18_y <= UInt<3>("h5")
    _T_21_x <= UInt<3>("h7")
    _T_21_y <= regC_y
    _T_26_x <= UInt<3>("h6")
    _T_26_y <= regB_y
    _T_29_x is invalid
    _T_29_y <= UInt<3>("h5")
