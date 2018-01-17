;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.7, sbtVersion: 0.13.12, builtAtString: 2017-03-31 17:47:22.700, builtAtMillis: 1490982442700
circuit TopModuleWithClks : 
  extmodule FakeBBClk : 
    output outClk : Clock[3]
    input inClk : Clock
    
    defname = FakeBBClk
    
    
  extmodule AsyncRegInit : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_1 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_2 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_3 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  module SEClkDivider : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip reset : UInt<1>, flip inClk : Clock, outClks : {3 : Clock, 1 : Clock, 0 : Clock}}
    
    io is invalid
    io is invalid
    inst AsyncRegInit of AsyncRegInit @[ClkDivider.scala 59:30]
    AsyncRegInit.out is invalid
    AsyncRegInit.in is invalid
    AsyncRegInit.init is invalid
    AsyncRegInit.reset is invalid
    AsyncRegInit.clk is invalid
    AsyncRegInit.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit.init <= UInt<1>("h01") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_1 of AsyncRegInit_1 @[ClkDivider.scala 59:30]
    AsyncRegInit_1.out is invalid
    AsyncRegInit_1.in is invalid
    AsyncRegInit_1.init is invalid
    AsyncRegInit_1.reset is invalid
    AsyncRegInit_1.clk is invalid
    AsyncRegInit_1.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_1.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_1.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_2 of AsyncRegInit_2 @[ClkDivider.scala 59:30]
    AsyncRegInit_2.out is invalid
    AsyncRegInit_2.in is invalid
    AsyncRegInit_2.init is invalid
    AsyncRegInit_2.reset is invalid
    AsyncRegInit_2.clk is invalid
    AsyncRegInit_2.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_2.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_2.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_3 of AsyncRegInit_3 @[ClkDivider.scala 59:30]
    AsyncRegInit_3.out is invalid
    AsyncRegInit_3.in is invalid
    AsyncRegInit_3.init is invalid
    AsyncRegInit_3.reset is invalid
    AsyncRegInit_3.clk is invalid
    AsyncRegInit_3.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_3.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_3.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    AsyncRegInit.in <= AsyncRegInit_3.out @[ClkDivider.scala 116:23]
    AsyncRegInit_1.in <= AsyncRegInit.out @[ClkDivider.scala 117:71]
    AsyncRegInit_2.in <= AsyncRegInit_1.out @[ClkDivider.scala 117:71]
    AsyncRegInit_3.in <= AsyncRegInit_2.out @[ClkDivider.scala 117:71]
    node _T_22 = asClock(AsyncRegInit.out) @[ClkDivider.scala 120:48]
    io.outClks.0 <= _T_22 @[ClkDivider.scala 120:25]
    node _T_23 = asClock(AsyncRegInit_1.out) @[ClkDivider.scala 120:48]
    io.outClks.1 <= _T_23 @[ClkDivider.scala 120:25]
    node _T_24 = asClock(AsyncRegInit_3.out) @[ClkDivider.scala 120:48]
    io.outClks.3 <= _T_24 @[ClkDivider.scala 120:25]
    
  module ModWithNestedClk : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inClk : Clock, bbOutClk : Clock[3], clkDivOut : Clock[3]}
    
    io is invalid
    io is invalid
    inst bb of FakeBBClk @[ClkGenSpec.scala 63:18]
    bb.outClk is invalid
    bb.inClk is invalid
    bb.inClk <= io.inClk @[ClkGenSpec.scala 64:15]
    io.bbOutClk[0] <= bb.outClk[0] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[1] <= bb.outClk[1] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[2] <= bb.outClk[2] @[ClkGenSpec.scala 65:15]
    inst clkDiv of SEClkDivider @[ClkGenSpec.scala 66:22]
    clkDiv.io is invalid
    clkDiv.clock <= clock
    clkDiv.reset <= reset
    clkDiv.io.reset <= reset @[ClkGenSpec.scala 67:19]
    clkDiv.io.inClk <= io.inClk @[ClkGenSpec.scala 68:19]
    io.clkDivOut[0] <= clkDiv.io.outClks.0 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[1] <= clkDiv.io.outClks.1 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[2] <= clkDiv.io.outClks.3 @[ClkGenSpec.scala 69:72]
    
  extmodule FakeBBClk_1 : 
    output outClk : Clock[3]
    input inClk : Clock
    
    defname = FakeBBClk
    
    
  extmodule AsyncRegInit_4 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_5 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_6 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_7 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  module SEClkDivider_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip reset : UInt<1>, flip inClk : Clock, outClks : {3 : Clock, 1 : Clock, 0 : Clock}}
    
    io is invalid
    io is invalid
    inst AsyncRegInit of AsyncRegInit_4 @[ClkDivider.scala 59:30]
    AsyncRegInit.out is invalid
    AsyncRegInit.in is invalid
    AsyncRegInit.init is invalid
    AsyncRegInit.reset is invalid
    AsyncRegInit.clk is invalid
    AsyncRegInit.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit.init <= UInt<1>("h01") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_1 of AsyncRegInit_5 @[ClkDivider.scala 59:30]
    AsyncRegInit_1.out is invalid
    AsyncRegInit_1.in is invalid
    AsyncRegInit_1.init is invalid
    AsyncRegInit_1.reset is invalid
    AsyncRegInit_1.clk is invalid
    AsyncRegInit_1.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_1.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_1.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_2 of AsyncRegInit_6 @[ClkDivider.scala 59:30]
    AsyncRegInit_2.out is invalid
    AsyncRegInit_2.in is invalid
    AsyncRegInit_2.init is invalid
    AsyncRegInit_2.reset is invalid
    AsyncRegInit_2.clk is invalid
    AsyncRegInit_2.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_2.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_2.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_3 of AsyncRegInit_7 @[ClkDivider.scala 59:30]
    AsyncRegInit_3.out is invalid
    AsyncRegInit_3.in is invalid
    AsyncRegInit_3.init is invalid
    AsyncRegInit_3.reset is invalid
    AsyncRegInit_3.clk is invalid
    AsyncRegInit_3.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_3.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_3.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    AsyncRegInit.in <= AsyncRegInit_3.out @[ClkDivider.scala 116:23]
    AsyncRegInit_1.in <= AsyncRegInit.out @[ClkDivider.scala 117:71]
    AsyncRegInit_2.in <= AsyncRegInit_1.out @[ClkDivider.scala 117:71]
    AsyncRegInit_3.in <= AsyncRegInit_2.out @[ClkDivider.scala 117:71]
    node _T_22 = asClock(AsyncRegInit_1.out) @[ClkDivider.scala 120:48]
    io.outClks.0 <= _T_22 @[ClkDivider.scala 120:25]
    node _T_23 = asClock(AsyncRegInit_2.out) @[ClkDivider.scala 120:48]
    io.outClks.1 <= _T_23 @[ClkDivider.scala 120:25]
    node _T_24 = asClock(AsyncRegInit.out) @[ClkDivider.scala 120:48]
    io.outClks.3 <= _T_24 @[ClkDivider.scala 120:25]
    
  module ModWithNestedClk_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inClk : Clock, bbOutClk : Clock[3], clkDivOut : Clock[3]}
    
    io is invalid
    io is invalid
    inst bb of FakeBBClk_1 @[ClkGenSpec.scala 63:18]
    bb.outClk is invalid
    bb.inClk is invalid
    bb.inClk <= io.inClk @[ClkGenSpec.scala 64:15]
    io.bbOutClk[0] <= bb.outClk[0] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[1] <= bb.outClk[1] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[2] <= bb.outClk[2] @[ClkGenSpec.scala 65:15]
    inst clkDiv of SEClkDivider_1 @[ClkGenSpec.scala 66:22]
    clkDiv.io is invalid
    clkDiv.clock <= clock
    clkDiv.reset <= reset
    clkDiv.io.reset <= reset @[ClkGenSpec.scala 67:19]
    clkDiv.io.inClk <= io.inClk @[ClkGenSpec.scala 68:19]
    io.clkDivOut[0] <= clkDiv.io.outClks.0 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[1] <= clkDiv.io.outClks.1 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[2] <= clkDiv.io.outClks.3 @[ClkGenSpec.scala 69:72]
    
  extmodule FakeBBClk_2 : 
    output outClk : Clock[3]
    input inClk : Clock
    
    defname = FakeBBClk
    
    
  extmodule AsyncRegInit_8 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_9 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_10 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  extmodule AsyncRegInit_11 : 
    output out : UInt<1>
    input in : UInt<1>
    input init : UInt<1>
    input reset : UInt<1>
    input clk : Clock
    
    defname = AsyncRegInit
    
    
  module SEClkDivider_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip reset : UInt<1>, flip inClk : Clock, outClks : {3 : Clock, 1 : Clock, 0 : Clock}}
    
    io is invalid
    io is invalid
    inst AsyncRegInit of AsyncRegInit_8 @[ClkDivider.scala 59:30]
    AsyncRegInit.out is invalid
    AsyncRegInit.in is invalid
    AsyncRegInit.init is invalid
    AsyncRegInit.reset is invalid
    AsyncRegInit.clk is invalid
    AsyncRegInit.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit.init <= UInt<1>("h01") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_1 of AsyncRegInit_9 @[ClkDivider.scala 59:30]
    AsyncRegInit_1.out is invalid
    AsyncRegInit_1.in is invalid
    AsyncRegInit_1.init is invalid
    AsyncRegInit_1.reset is invalid
    AsyncRegInit_1.clk is invalid
    AsyncRegInit_1.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_1.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_1.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_2 of AsyncRegInit_10 @[ClkDivider.scala 59:30]
    AsyncRegInit_2.out is invalid
    AsyncRegInit_2.in is invalid
    AsyncRegInit_2.init is invalid
    AsyncRegInit_2.reset is invalid
    AsyncRegInit_2.clk is invalid
    AsyncRegInit_2.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_2.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_2.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    inst AsyncRegInit_3 of AsyncRegInit_11 @[ClkDivider.scala 59:30]
    AsyncRegInit_3.out is invalid
    AsyncRegInit_3.in is invalid
    AsyncRegInit_3.init is invalid
    AsyncRegInit_3.reset is invalid
    AsyncRegInit_3.clk is invalid
    AsyncRegInit_3.clk <= io.inClk @[ClkDivider.scala 60:25]
    AsyncRegInit_3.reset <= io.reset @[ClkDivider.scala 61:27]
    AsyncRegInit_3.init <= UInt<1>("h00") @[ClkDivider.scala 62:26]
    AsyncRegInit.in <= AsyncRegInit_3.out @[ClkDivider.scala 116:23]
    AsyncRegInit_1.in <= AsyncRegInit.out @[ClkDivider.scala 117:71]
    AsyncRegInit_2.in <= AsyncRegInit_1.out @[ClkDivider.scala 117:71]
    AsyncRegInit_3.in <= AsyncRegInit_2.out @[ClkDivider.scala 117:71]
    node _T_22 = asClock(AsyncRegInit_1.out) @[ClkDivider.scala 120:48]
    io.outClks.0 <= _T_22 @[ClkDivider.scala 120:25]
    node _T_23 = asClock(AsyncRegInit_2.out) @[ClkDivider.scala 120:48]
    io.outClks.1 <= _T_23 @[ClkDivider.scala 120:25]
    node _T_24 = asClock(AsyncRegInit.out) @[ClkDivider.scala 120:48]
    io.outClks.3 <= _T_24 @[ClkDivider.scala 120:25]
    
  module ModWithNestedClk_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inClk : Clock, bbOutClk : Clock[3], clkDivOut : Clock[3]}
    
    io is invalid
    io is invalid
    inst bb of FakeBBClk_2 @[ClkGenSpec.scala 63:18]
    bb.outClk is invalid
    bb.inClk is invalid
    bb.inClk <= io.inClk @[ClkGenSpec.scala 64:15]
    io.bbOutClk[0] <= bb.outClk[0] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[1] <= bb.outClk[1] @[ClkGenSpec.scala 65:15]
    io.bbOutClk[2] <= bb.outClk[2] @[ClkGenSpec.scala 65:15]
    inst clkDiv of SEClkDivider_2 @[ClkGenSpec.scala 66:22]
    clkDiv.io is invalid
    clkDiv.clock <= clock
    clkDiv.reset <= reset
    clkDiv.io.reset <= reset @[ClkGenSpec.scala 67:19]
    clkDiv.io.inClk <= io.inClk @[ClkGenSpec.scala 68:19]
    io.clkDivOut[0] <= clkDiv.io.outClks.0 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[1] <= clkDiv.io.outClks.1 @[ClkGenSpec.scala 69:72]
    io.clkDivOut[2] <= clkDiv.io.outClks.3 @[ClkGenSpec.scala 69:72]
    
  module TopModuleWithClks : 
    input clock : Clock
    input reset : UInt<1>
    output io : {gen1 : {bbOutClk : UInt<1>[3], clkDivOut : UInt<1>[3]}, gen2 : {bbOutClk : UInt<1>[3], clkDivOut : UInt<1>[3]}, gen3 : {bbOutClk : UInt<1>[3], clkDivOut : UInt<1>[3]}, flip fakeClk1 : Clock, flip fakeClk2 : Clock}
    
    io is invalid
    io is invalid
    inst gen1 of ModWithNestedClk @[ClkGenSpec.scala 91:20]
    gen1.io is invalid
    gen1.clock <= clock
    gen1.reset <= reset
    node _T_63 = asUInt(gen1.io.bbOutClk[0]) @[ClkGenSpec.scala 92:55]
    node _T_64 = asUInt(gen1.io.bbOutClk[1]) @[ClkGenSpec.scala 92:55]
    node _T_65 = asUInt(gen1.io.bbOutClk[2]) @[ClkGenSpec.scala 92:55]
    wire _T_68 : UInt<1>[3] @[ClkGenSpec.scala 92:26]
    _T_68 is invalid @[ClkGenSpec.scala 92:26]
    _T_68[0] <= _T_63 @[ClkGenSpec.scala 92:26]
    _T_68[1] <= _T_64 @[ClkGenSpec.scala 92:26]
    _T_68[2] <= _T_65 @[ClkGenSpec.scala 92:26]
    io.gen1.bbOutClk[0] <= _T_68[0] @[ClkGenSpec.scala 92:20]
    io.gen1.bbOutClk[1] <= _T_68[1] @[ClkGenSpec.scala 92:20]
    io.gen1.bbOutClk[2] <= _T_68[2] @[ClkGenSpec.scala 92:20]
    node _T_74 = asUInt(gen1.io.clkDivOut[0]) @[ClkGenSpec.scala 93:57]
    node _T_75 = asUInt(gen1.io.clkDivOut[1]) @[ClkGenSpec.scala 93:57]
    node _T_76 = asUInt(gen1.io.clkDivOut[2]) @[ClkGenSpec.scala 93:57]
    wire _T_79 : UInt<1>[3] @[ClkGenSpec.scala 93:27]
    _T_79 is invalid @[ClkGenSpec.scala 93:27]
    _T_79[0] <= _T_74 @[ClkGenSpec.scala 93:27]
    _T_79[1] <= _T_75 @[ClkGenSpec.scala 93:27]
    _T_79[2] <= _T_76 @[ClkGenSpec.scala 93:27]
    io.gen1.clkDivOut[0] <= _T_79[0] @[ClkGenSpec.scala 93:21]
    io.gen1.clkDivOut[1] <= _T_79[1] @[ClkGenSpec.scala 93:21]
    io.gen1.clkDivOut[2] <= _T_79[2] @[ClkGenSpec.scala 93:21]
    gen1.io.inClk <= clock @[ClkGenSpec.scala 94:17]
    inst gen2 of ModWithNestedClk_1 @[ClkGenSpec.scala 96:20]
    gen2.io is invalid
    gen2.clock <= clock
    gen2.reset <= reset
    node _T_85 = asUInt(gen2.io.bbOutClk[0]) @[ClkGenSpec.scala 97:55]
    node _T_86 = asUInt(gen2.io.bbOutClk[1]) @[ClkGenSpec.scala 97:55]
    node _T_87 = asUInt(gen2.io.bbOutClk[2]) @[ClkGenSpec.scala 97:55]
    wire _T_90 : UInt<1>[3] @[ClkGenSpec.scala 97:26]
    _T_90 is invalid @[ClkGenSpec.scala 97:26]
    _T_90[0] <= _T_85 @[ClkGenSpec.scala 97:26]
    _T_90[1] <= _T_86 @[ClkGenSpec.scala 97:26]
    _T_90[2] <= _T_87 @[ClkGenSpec.scala 97:26]
    io.gen2.bbOutClk[0] <= _T_90[0] @[ClkGenSpec.scala 97:20]
    io.gen2.bbOutClk[1] <= _T_90[1] @[ClkGenSpec.scala 97:20]
    io.gen2.bbOutClk[2] <= _T_90[2] @[ClkGenSpec.scala 97:20]
    node _T_96 = asUInt(gen2.io.clkDivOut[0]) @[ClkGenSpec.scala 98:57]
    node _T_97 = asUInt(gen2.io.clkDivOut[1]) @[ClkGenSpec.scala 98:57]
    node _T_98 = asUInt(gen2.io.clkDivOut[2]) @[ClkGenSpec.scala 98:57]
    wire _T_101 : UInt<1>[3] @[ClkGenSpec.scala 98:27]
    _T_101 is invalid @[ClkGenSpec.scala 98:27]
    _T_101[0] <= _T_96 @[ClkGenSpec.scala 98:27]
    _T_101[1] <= _T_97 @[ClkGenSpec.scala 98:27]
    _T_101[2] <= _T_98 @[ClkGenSpec.scala 98:27]
    io.gen2.clkDivOut[0] <= _T_101[0] @[ClkGenSpec.scala 98:21]
    io.gen2.clkDivOut[1] <= _T_101[1] @[ClkGenSpec.scala 98:21]
    io.gen2.clkDivOut[2] <= _T_101[2] @[ClkGenSpec.scala 98:21]
    gen2.io.inClk <= gen1.io.clkDivOut[2] @[ClkGenSpec.scala 99:17]
    inst gen3 of ModWithNestedClk_2 @[ClkGenSpec.scala 100:20]
    gen3.io is invalid
    gen3.clock <= clock
    gen3.reset <= reset
    node _T_107 = asUInt(gen3.io.bbOutClk[0]) @[ClkGenSpec.scala 101:55]
    node _T_108 = asUInt(gen3.io.bbOutClk[1]) @[ClkGenSpec.scala 101:55]
    node _T_109 = asUInt(gen3.io.bbOutClk[2]) @[ClkGenSpec.scala 101:55]
    wire _T_112 : UInt<1>[3] @[ClkGenSpec.scala 101:26]
    _T_112 is invalid @[ClkGenSpec.scala 101:26]
    _T_112[0] <= _T_107 @[ClkGenSpec.scala 101:26]
    _T_112[1] <= _T_108 @[ClkGenSpec.scala 101:26]
    _T_112[2] <= _T_109 @[ClkGenSpec.scala 101:26]
    io.gen3.bbOutClk[0] <= _T_112[0] @[ClkGenSpec.scala 101:20]
    io.gen3.bbOutClk[1] <= _T_112[1] @[ClkGenSpec.scala 101:20]
    io.gen3.bbOutClk[2] <= _T_112[2] @[ClkGenSpec.scala 101:20]
    node _T_118 = asUInt(gen3.io.clkDivOut[0]) @[ClkGenSpec.scala 102:57]
    node _T_119 = asUInt(gen3.io.clkDivOut[1]) @[ClkGenSpec.scala 102:57]
    node _T_120 = asUInt(gen3.io.clkDivOut[2]) @[ClkGenSpec.scala 102:57]
    wire _T_123 : UInt<1>[3] @[ClkGenSpec.scala 102:27]
    _T_123 is invalid @[ClkGenSpec.scala 102:27]
    _T_123[0] <= _T_118 @[ClkGenSpec.scala 102:27]
    _T_123[1] <= _T_119 @[ClkGenSpec.scala 102:27]
    _T_123[2] <= _T_120 @[ClkGenSpec.scala 102:27]
    io.gen3.clkDivOut[0] <= _T_123[0] @[ClkGenSpec.scala 102:21]
    io.gen3.clkDivOut[1] <= _T_123[1] @[ClkGenSpec.scala 102:21]
    io.gen3.clkDivOut[2] <= _T_123[2] @[ClkGenSpec.scala 102:21]
    gen3.io.inClk <= gen1.io.clkDivOut[2] @[ClkGenSpec.scala 103:17]
    
