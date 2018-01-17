circuit GCD :
  module GCD :
    input clk : Clock
    input reset : UInt<1>
    input io_a : UInt<16>
    input io_b : UInt<16>
    input io_e : UInt<1>
    output io_z : UInt<16>
    output io_v : UInt<1>
    skip
    reg x : UInt<16>, clk with :
      reset => (UInt<1>("h0"), x)
    reg y : UInt<16>, clk with :
      reset => (UInt<1>("h0"), y)
    node T_7 = gt(x, y)
    node T_8 = sub(x, y)
    node T_9 = tail(T_8, 1)
    node GEN_0 = mux(T_7, T_9, x)
    node T_10 = gt(x, y)
    node T_12 = eq(T_10, UInt<1>("h0"))
    node T_13 = sub(y, x)
    node T_14 = tail(T_13, 1)
    node GEN_1 = mux(T_12, T_14, y)
    node GEN_2 = mux(io_e, io_a, GEN_0)
    node GEN_3 = mux(io_e, io_b, GEN_1)
    skip
    node T_16 = eq(y, UInt<1>("h0"))
    skip
    io_z <= x
    io_v <= T_16
    x <= GEN_2
    y <= GEN_3
