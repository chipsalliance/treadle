circuit HasLoop :
  module HasLoop :
    input clk : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<16>, flip b : UInt<16>, flip e : UInt<1>, z : UInt<16>, v : UInt<1>}
    
    io is invalid

    wire T_8 : UInt<16>
    node T_7 = and(T_8, io.a)
    T_8  <= and(T_7, io.a)
    io.z <= and(T_7, T_8)

    io.v <= UInt<1>("h01")
