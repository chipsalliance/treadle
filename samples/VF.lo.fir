circuit VF : @[:@2.0]
  module VF : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_addr : UInt<8> @[:@6.4]
    output io_value : UInt<8> @[:@6.4]
  
    wire vec_0 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_1 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_2 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_3 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_4 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_5 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_6 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_7 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_8 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_9 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire vec_10 : UInt<8> @[VecFillSpec.scala 15:17:@11.4]
    wire _T_33_0 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_1 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_2 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_3 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_4 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_5 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_6 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_7 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_8 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_9 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    wire _T_33_10 : UInt<4> @[VecFillSpec.scala 16:13:@13.4]
    node _T_48 = bits(io_addr, 3, 0) @[:@37.4]
    wire _GEN_0 : UInt<8> @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_1 = mux(and(UInt<1>("h1"), eq(UInt<1>("h1"), _T_48)), vec_1, vec_0) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_2 = mux(and(UInt<1>("h1"), eq(UInt<2>("h2"), _T_48)), vec_2, _GEN_1) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_3 = mux(and(UInt<1>("h1"), eq(UInt<2>("h3"), _T_48)), vec_3, _GEN_2) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_4 = mux(and(UInt<1>("h1"), eq(UInt<3>("h4"), _T_48)), vec_4, _GEN_3) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_5 = mux(and(UInt<1>("h1"), eq(UInt<3>("h5"), _T_48)), vec_5, _GEN_4) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_6 = mux(and(UInt<1>("h1"), eq(UInt<3>("h6"), _T_48)), vec_6, _GEN_5) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_7 = mux(and(UInt<1>("h1"), eq(UInt<3>("h7"), _T_48)), vec_7, _GEN_6) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_8 = mux(and(UInt<1>("h1"), eq(UInt<4>("h8"), _T_48)), vec_8, _GEN_7) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_9 = mux(and(UInt<1>("h1"), eq(UInt<4>("h9"), _T_48)), vec_9, _GEN_8) @[VecFillSpec.scala 18:12:@38.4]
    node _GEN_10 = mux(and(UInt<1>("h1"), eq(UInt<4>("ha"), _T_48)), vec_10, _GEN_9) @[VecFillSpec.scala 18:12:@38.4]
    io_value <= _GEN_0
    vec_0 <= _T_33_0
    vec_1 <= _T_33_1
    vec_2 <= _T_33_2
    vec_3 <= _T_33_3
    vec_4 <= _T_33_4
    vec_5 <= _T_33_5
    vec_6 <= _T_33_6
    vec_7 <= _T_33_7
    vec_8 <= _T_33_8
    vec_9 <= _T_33_9
    vec_10 <= _T_33_10
    _T_33_0 <= UInt<1>("h0")
    _T_33_1 <= UInt<1>("h1")
    _T_33_2 <= UInt<2>("h2")
    _T_33_3 <= UInt<2>("h3")
    _T_33_4 <= UInt<3>("h4")
    _T_33_5 <= UInt<3>("h5")
    _T_33_6 <= UInt<3>("h6")
    _T_33_7 <= UInt<3>("h7")
    _T_33_8 <= UInt<4>("h8")
    _T_33_9 <= UInt<4>("h9")
    _T_33_10 <= UInt<4>("ha")
    _GEN_0 <= _GEN_10
