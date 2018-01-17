;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit MultiGcdCalculator : 
  module GcdEngine : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip decoupledInput : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, validOutput : {valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>, gcd : UInt<16>}}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 38:14]
    reg y : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 39:14]
    reg xOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 40:22]
    reg yOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 41:22]
    reg busy : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[ConcurrentDecoupledTestingSpec.scala 42:21]
    node _T_20 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 44:30]
    io.decoupledInput.ready <= _T_20 @[ConcurrentDecoupledTestingSpec.scala 44:27]
    node _T_22 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 46:35]
    node _T_23 = and(io.decoupledInput.valid, _T_22) @[ConcurrentDecoupledTestingSpec.scala 46:32]
    when _T_23 : @[ConcurrentDecoupledTestingSpec.scala 46:42]
      x <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 47:7]
      y <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 48:7]
      xOriginal <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 49:15]
      yOriginal <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 50:15]
      busy <= UInt<1>("h01") @[ConcurrentDecoupledTestingSpec.scala 51:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 46:42]
    when busy : @[ConcurrentDecoupledTestingSpec.scala 54:14]
      node _T_25 = gt(x, y) @[ConcurrentDecoupledTestingSpec.scala 55:12]
      when _T_25 : @[ConcurrentDecoupledTestingSpec.scala 55:17]
        x <= y @[ConcurrentDecoupledTestingSpec.scala 56:9]
        y <= x @[ConcurrentDecoupledTestingSpec.scala 57:9]
        skip @[ConcurrentDecoupledTestingSpec.scala 55:17]
      node _T_27 = eq(_T_25, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 55:17]
      when _T_27 : @[ConcurrentDecoupledTestingSpec.scala 59:18]
        node _T_28 = sub(y, x) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_29 = asUInt(_T_28) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_30 = tail(_T_29, 1) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        y <= _T_30 @[ConcurrentDecoupledTestingSpec.scala 60:11]
        skip @[ConcurrentDecoupledTestingSpec.scala 59:18]
      skip @[ConcurrentDecoupledTestingSpec.scala 54:14]
    io.validOutput.bits.gcd <= x @[ConcurrentDecoupledTestingSpec.scala 64:27]
    io.validOutput.bits.a <= xOriginal @[ConcurrentDecoupledTestingSpec.scala 65:27]
    io.validOutput.bits.b <= yOriginal @[ConcurrentDecoupledTestingSpec.scala 66:27]
    node _T_32 = eq(y, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 67:32]
    node _T_33 = and(_T_32, busy) @[ConcurrentDecoupledTestingSpec.scala 67:40]
    io.validOutput.valid <= _T_33 @[ConcurrentDecoupledTestingSpec.scala 67:27]
    when io.validOutput.valid : @[ConcurrentDecoupledTestingSpec.scala 68:30]
      busy <= UInt<1>("h00") @[ConcurrentDecoupledTestingSpec.scala 69:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 68:30]
    
  module GcdEngine_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip decoupledInput : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, validOutput : {valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>, gcd : UInt<16>}}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 38:14]
    reg y : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 39:14]
    reg xOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 40:22]
    reg yOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 41:22]
    reg busy : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[ConcurrentDecoupledTestingSpec.scala 42:21]
    node _T_20 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 44:30]
    io.decoupledInput.ready <= _T_20 @[ConcurrentDecoupledTestingSpec.scala 44:27]
    node _T_22 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 46:35]
    node _T_23 = and(io.decoupledInput.valid, _T_22) @[ConcurrentDecoupledTestingSpec.scala 46:32]
    when _T_23 : @[ConcurrentDecoupledTestingSpec.scala 46:42]
      x <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 47:7]
      y <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 48:7]
      xOriginal <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 49:15]
      yOriginal <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 50:15]
      busy <= UInt<1>("h01") @[ConcurrentDecoupledTestingSpec.scala 51:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 46:42]
    when busy : @[ConcurrentDecoupledTestingSpec.scala 54:14]
      node _T_25 = gt(x, y) @[ConcurrentDecoupledTestingSpec.scala 55:12]
      when _T_25 : @[ConcurrentDecoupledTestingSpec.scala 55:17]
        x <= y @[ConcurrentDecoupledTestingSpec.scala 56:9]
        y <= x @[ConcurrentDecoupledTestingSpec.scala 57:9]
        skip @[ConcurrentDecoupledTestingSpec.scala 55:17]
      node _T_27 = eq(_T_25, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 55:17]
      when _T_27 : @[ConcurrentDecoupledTestingSpec.scala 59:18]
        node _T_28 = sub(y, x) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_29 = asUInt(_T_28) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_30 = tail(_T_29, 1) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        y <= _T_30 @[ConcurrentDecoupledTestingSpec.scala 60:11]
        skip @[ConcurrentDecoupledTestingSpec.scala 59:18]
      skip @[ConcurrentDecoupledTestingSpec.scala 54:14]
    io.validOutput.bits.gcd <= x @[ConcurrentDecoupledTestingSpec.scala 64:27]
    io.validOutput.bits.a <= xOriginal @[ConcurrentDecoupledTestingSpec.scala 65:27]
    io.validOutput.bits.b <= yOriginal @[ConcurrentDecoupledTestingSpec.scala 66:27]
    node _T_32 = eq(y, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 67:32]
    node _T_33 = and(_T_32, busy) @[ConcurrentDecoupledTestingSpec.scala 67:40]
    io.validOutput.valid <= _T_33 @[ConcurrentDecoupledTestingSpec.scala 67:27]
    when io.validOutput.valid : @[ConcurrentDecoupledTestingSpec.scala 68:30]
      busy <= UInt<1>("h00") @[ConcurrentDecoupledTestingSpec.scala 69:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 68:30]
    
  module GcdEngine_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip decoupledInput : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, validOutput : {valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>, gcd : UInt<16>}}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 38:14]
    reg y : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 39:14]
    reg xOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 40:22]
    reg yOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 41:22]
    reg busy : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[ConcurrentDecoupledTestingSpec.scala 42:21]
    node _T_20 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 44:30]
    io.decoupledInput.ready <= _T_20 @[ConcurrentDecoupledTestingSpec.scala 44:27]
    node _T_22 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 46:35]
    node _T_23 = and(io.decoupledInput.valid, _T_22) @[ConcurrentDecoupledTestingSpec.scala 46:32]
    when _T_23 : @[ConcurrentDecoupledTestingSpec.scala 46:42]
      x <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 47:7]
      y <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 48:7]
      xOriginal <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 49:15]
      yOriginal <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 50:15]
      busy <= UInt<1>("h01") @[ConcurrentDecoupledTestingSpec.scala 51:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 46:42]
    when busy : @[ConcurrentDecoupledTestingSpec.scala 54:14]
      node _T_25 = gt(x, y) @[ConcurrentDecoupledTestingSpec.scala 55:12]
      when _T_25 : @[ConcurrentDecoupledTestingSpec.scala 55:17]
        x <= y @[ConcurrentDecoupledTestingSpec.scala 56:9]
        y <= x @[ConcurrentDecoupledTestingSpec.scala 57:9]
        skip @[ConcurrentDecoupledTestingSpec.scala 55:17]
      node _T_27 = eq(_T_25, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 55:17]
      when _T_27 : @[ConcurrentDecoupledTestingSpec.scala 59:18]
        node _T_28 = sub(y, x) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_29 = asUInt(_T_28) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_30 = tail(_T_29, 1) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        y <= _T_30 @[ConcurrentDecoupledTestingSpec.scala 60:11]
        skip @[ConcurrentDecoupledTestingSpec.scala 59:18]
      skip @[ConcurrentDecoupledTestingSpec.scala 54:14]
    io.validOutput.bits.gcd <= x @[ConcurrentDecoupledTestingSpec.scala 64:27]
    io.validOutput.bits.a <= xOriginal @[ConcurrentDecoupledTestingSpec.scala 65:27]
    io.validOutput.bits.b <= yOriginal @[ConcurrentDecoupledTestingSpec.scala 66:27]
    node _T_32 = eq(y, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 67:32]
    node _T_33 = and(_T_32, busy) @[ConcurrentDecoupledTestingSpec.scala 67:40]
    io.validOutput.valid <= _T_33 @[ConcurrentDecoupledTestingSpec.scala 67:27]
    when io.validOutput.valid : @[ConcurrentDecoupledTestingSpec.scala 68:30]
      busy <= UInt<1>("h00") @[ConcurrentDecoupledTestingSpec.scala 69:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 68:30]
    
  module GcdEngine_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip decoupledInput : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}, validOutput : {valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>, gcd : UInt<16>}}}
    
    clock is invalid
    reset is invalid
    io is invalid
    reg x : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 38:14]
    reg y : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 39:14]
    reg xOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 40:22]
    reg yOriginal : UInt<16>, clock @[ConcurrentDecoupledTestingSpec.scala 41:22]
    reg busy : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[ConcurrentDecoupledTestingSpec.scala 42:21]
    node _T_20 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 44:30]
    io.decoupledInput.ready <= _T_20 @[ConcurrentDecoupledTestingSpec.scala 44:27]
    node _T_22 = eq(busy, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 46:35]
    node _T_23 = and(io.decoupledInput.valid, _T_22) @[ConcurrentDecoupledTestingSpec.scala 46:32]
    when _T_23 : @[ConcurrentDecoupledTestingSpec.scala 46:42]
      x <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 47:7]
      y <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 48:7]
      xOriginal <= io.decoupledInput.bits.a @[ConcurrentDecoupledTestingSpec.scala 49:15]
      yOriginal <= io.decoupledInput.bits.b @[ConcurrentDecoupledTestingSpec.scala 50:15]
      busy <= UInt<1>("h01") @[ConcurrentDecoupledTestingSpec.scala 51:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 46:42]
    when busy : @[ConcurrentDecoupledTestingSpec.scala 54:14]
      node _T_25 = gt(x, y) @[ConcurrentDecoupledTestingSpec.scala 55:12]
      when _T_25 : @[ConcurrentDecoupledTestingSpec.scala 55:17]
        x <= y @[ConcurrentDecoupledTestingSpec.scala 56:9]
        y <= x @[ConcurrentDecoupledTestingSpec.scala 57:9]
        skip @[ConcurrentDecoupledTestingSpec.scala 55:17]
      node _T_27 = eq(_T_25, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 55:17]
      when _T_27 : @[ConcurrentDecoupledTestingSpec.scala 59:18]
        node _T_28 = sub(y, x) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_29 = asUInt(_T_28) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        node _T_30 = tail(_T_29, 1) @[ConcurrentDecoupledTestingSpec.scala 60:16]
        y <= _T_30 @[ConcurrentDecoupledTestingSpec.scala 60:11]
        skip @[ConcurrentDecoupledTestingSpec.scala 59:18]
      skip @[ConcurrentDecoupledTestingSpec.scala 54:14]
    io.validOutput.bits.gcd <= x @[ConcurrentDecoupledTestingSpec.scala 64:27]
    io.validOutput.bits.a <= xOriginal @[ConcurrentDecoupledTestingSpec.scala 65:27]
    io.validOutput.bits.b <= yOriginal @[ConcurrentDecoupledTestingSpec.scala 66:27]
    node _T_32 = eq(y, UInt<1>("h00")) @[ConcurrentDecoupledTestingSpec.scala 67:32]
    node _T_33 = and(_T_32, busy) @[ConcurrentDecoupledTestingSpec.scala 67:40]
    io.validOutput.valid <= _T_33 @[ConcurrentDecoupledTestingSpec.scala 67:27]
    when io.validOutput.valid : @[ConcurrentDecoupledTestingSpec.scala 68:30]
      busy <= UInt<1>("h00") @[ConcurrentDecoupledTestingSpec.scala 69:10]
      skip @[ConcurrentDecoupledTestingSpec.scala 68:30]
    
  module MultiGcdCalculator : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input : {flip ready : UInt<1>, valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>}}[4], output : {valid : UInt<1>, bits : {a : UInt<16>, b : UInt<16>, gcd : UInt<16>}}[4]}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst GcdEngine of GcdEngine @[ConcurrentDecoupledTestingSpec.scala 84:24]
    GcdEngine.io is invalid
    GcdEngine.clock <= clock
    GcdEngine.reset <= reset
    GcdEngine.io.decoupledInput.bits.b <= io.input[0].bits.b @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine.io.decoupledInput.bits.a <= io.input[0].bits.a @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine.io.decoupledInput.valid <= io.input[0].valid @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.input[0].ready <= GcdEngine.io.decoupledInput.ready @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.output[0].bits.gcd <= GcdEngine.io.validOutput.bits.gcd @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[0].bits.b <= GcdEngine.io.validOutput.bits.b @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[0].bits.a <= GcdEngine.io.validOutput.bits.a @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[0].valid <= GcdEngine.io.validOutput.valid @[ConcurrentDecoupledTestingSpec.scala 86:27]
    inst GcdEngine_1 of GcdEngine_1 @[ConcurrentDecoupledTestingSpec.scala 84:24]
    GcdEngine_1.io is invalid
    GcdEngine_1.clock <= clock
    GcdEngine_1.reset <= reset
    GcdEngine_1.io.decoupledInput.bits.b <= io.input[1].bits.b @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_1.io.decoupledInput.bits.a <= io.input[1].bits.a @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_1.io.decoupledInput.valid <= io.input[1].valid @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.input[1].ready <= GcdEngine_1.io.decoupledInput.ready @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.output[1].bits.gcd <= GcdEngine_1.io.validOutput.bits.gcd @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[1].bits.b <= GcdEngine_1.io.validOutput.bits.b @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[1].bits.a <= GcdEngine_1.io.validOutput.bits.a @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[1].valid <= GcdEngine_1.io.validOutput.valid @[ConcurrentDecoupledTestingSpec.scala 86:27]
    inst GcdEngine_2 of GcdEngine_2 @[ConcurrentDecoupledTestingSpec.scala 84:24]
    GcdEngine_2.io is invalid
    GcdEngine_2.clock <= clock
    GcdEngine_2.reset <= reset
    GcdEngine_2.io.decoupledInput.bits.b <= io.input[2].bits.b @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_2.io.decoupledInput.bits.a <= io.input[2].bits.a @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_2.io.decoupledInput.valid <= io.input[2].valid @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.input[2].ready <= GcdEngine_2.io.decoupledInput.ready @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.output[2].bits.gcd <= GcdEngine_2.io.validOutput.bits.gcd @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[2].bits.b <= GcdEngine_2.io.validOutput.bits.b @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[2].bits.a <= GcdEngine_2.io.validOutput.bits.a @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[2].valid <= GcdEngine_2.io.validOutput.valid @[ConcurrentDecoupledTestingSpec.scala 86:27]
    inst GcdEngine_3 of GcdEngine_3 @[ConcurrentDecoupledTestingSpec.scala 84:24]
    GcdEngine_3.io is invalid
    GcdEngine_3.clock <= clock
    GcdEngine_3.reset <= reset
    GcdEngine_3.io.decoupledInput.bits.b <= io.input[3].bits.b @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_3.io.decoupledInput.bits.a <= io.input[3].bits.a @[ConcurrentDecoupledTestingSpec.scala 85:30]
    GcdEngine_3.io.decoupledInput.valid <= io.input[3].valid @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.input[3].ready <= GcdEngine_3.io.decoupledInput.ready @[ConcurrentDecoupledTestingSpec.scala 85:30]
    io.output[3].bits.gcd <= GcdEngine_3.io.validOutput.bits.gcd @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[3].bits.b <= GcdEngine_3.io.validOutput.bits.b @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[3].bits.a <= GcdEngine_3.io.validOutput.bits.a @[ConcurrentDecoupledTestingSpec.scala 86:27]
    io.output[3].valid <= GcdEngine_3.io.validOutput.valid @[ConcurrentDecoupledTestingSpec.scala 86:27]
    
