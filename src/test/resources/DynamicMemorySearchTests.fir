circuit DynamicMemorySearch : 
  module DynamicMemorySearch : 
    input clk : Clock
    input reset : UInt<1>
    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<4>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
    
    io is invalid
    reg index : UInt<3>, clk with : (reset => (reset, UInt<3>("h00")))
    cmem list : UInt<4>[8]
    infer mport memVal = list[index], clk
    node T_10 = eq(io.en, UInt<1>("h00"))
    node T_11 = eq(memVal, io.data)
    node T_13 = eq(index, UInt<3>("h07"))
    node T_14 = or(T_11, T_13)
    node over = and(T_10, T_14)
    when reset :
      infer mport T_16 = list[UInt<1>("h00")], clk
      T_16 <= UInt<1>("h00")
      infer mport T_19 = list[UInt<1>("h01")], clk
      T_19 <= UInt<1>("h00")
      infer mport T_22 = list[UInt<2>("h02")], clk
      T_22 <= UInt<1>("h00")
      infer mport T_25 = list[UInt<2>("h03")], clk
      T_25 <= UInt<1>("h00")
      infer mport T_28 = list[UInt<3>("h04")], clk
      T_28 <= UInt<1>("h00")
      infer mport T_31 = list[UInt<3>("h05")], clk
      T_31 <= UInt<1>("h00")
      infer mport T_34 = list[UInt<3>("h06")], clk
      T_34 <= UInt<1>("h00")
      infer mport T_37 = list[UInt<3>("h07")], clk
      T_37 <= UInt<1>("h00")
      skip
    when io.isWr :
      infer mport T_39 = list[io.wrAddr], clk
      T_39 <= io.data
      skip
    node T_41 = eq(io.isWr, UInt<1>("h00"))
    node T_42 = and(T_41, io.en)
    when T_42 :
      index <= UInt<1>("h00")
      skip
    node T_45 = eq(over, UInt<1>("h00"))
    node T_47 = eq(io.isWr, UInt<1>("h00"))
    node T_49 = eq(io.en, UInt<1>("h00"))
    node T_50 = and(T_47, T_49)
    node T_51 = and(T_50, T_45)
    when T_51 :
      node T_53 = add(index, UInt<1>("h01"))
      node T_54 = tail(T_53, 1)
      index <= T_54
      skip
    io.done <= over
    io.target <= index
    
