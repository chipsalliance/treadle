circuit test :
  module test :
    input metaReset : UInt<1>
    input clock : Clock
    input reset : UInt<1>
    input cond : UInt<1>
    output out : UInt<32>
    output assert_failed : UInt<1>

    node _GEN_0 = mux(cond, UInt<4>("h8"), UInt<3>("h7"))
    out <= _GEN_0
    reg prev_reset : UInt<1>, clock with :
      reset => (UInt<1>("h0"), prev_reset)
    prev_reset <= mux(metaReset, UInt<1>("h0"), reset)
    node cond_s = cond
    reg cond_prev : UInt<1>, clock with :
      reset => (UInt<1>("h0"), cond_prev)
    cond_prev <= mux(metaReset, UInt<1>("h0"), cond_s)
    node cond_toggle = xor(cond_s, cond_prev)
    cover(clock, cond_toggle, not(or(reset, prev_reset)), "") : cond_toggleNoReset
    assert_failed <= UInt<1>("h0")
