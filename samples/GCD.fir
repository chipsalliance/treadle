circuit GCD :
  module GCD :
    input clk : Clock
    input reset : UInt<1>
    input io_a : UInt<16>
    input io_b : UInt<16>
    input io_e : UInt<1>
    output io_z : UInt<16>
    output io_v : UInt<1>

    reg x : UInt<16>, clk with :
      reset => (UInt<1>("h0"), x)
    reg y : UInt<16>, clk with :
      reset => (UInt<1>("h0"), y)
    node T_13 = gt(x, y)
    node T_14 = sub(x, y)
    node T_15 = tail(T_14, 1)
    node T_17 = eq(T_13, UInt<1>("h0"))
    node T_18 = sub(y, x)
    node T_19 = tail(T_18, 1)
    node T_21 = eq(y, UInt<1>("h0"))
    node GEN_0 = mux(T_13, T_15, x)
    node GEN_1 = mux(T_17, T_19, y)
    io_z <= x
    io_v <= T_21
    x <= mux(io_e, io_a, GEN_0)
    y <= mux(io_e, io_b, GEN_1)

