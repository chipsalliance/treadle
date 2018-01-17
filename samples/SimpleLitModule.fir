;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.16, builtAtString: 2017-09-16 03:49:13.973, builtAtMillis: 1505533753973
circuit SimpleLitModule : 
  module SimpleLitModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip i : {r : {node : UInt<64>}, b : UInt<1>, cGenL : {real : Fixed<16><<8>>, imag : Fixed<16><<8>>}, cFS : {real : Fixed<8><<4>>, imag : Fixed<8><<4>>}, cR : {real : {node : UInt<64>}, imag : {node : UInt<64>}}, short : {gen : Fixed<8><<4>>, s : SInt<8>, f : Fixed<8><<4>>, u : UInt<8>}, long : {gen : Fixed<16><<8>>, s : SInt<16>, f : Fixed<16><<8>>, u : UInt<16>}, vU : UInt<8>[10], vS : SInt<8>[10], vF : Fixed<8><<4>>[10]}, o : {r : {node : UInt<64>}, b : UInt<1>, cGenL : {real : Fixed<16><<8>>, imag : Fixed<16><<8>>}, cFS : {real : Fixed<8><<4>>, imag : Fixed<8><<4>>}, cR : {real : {node : UInt<64>}, imag : {node : UInt<64>}}, short : {gen : Fixed<8><<4>>, s : SInt<8>, f : Fixed<8><<4>>, u : UInt<8>}, long : {gen : Fixed<16><<8>>, s : SInt<16>, f : Fixed<16><<8>>, u : UInt<16>}, vU : UInt<8>[10], vS : SInt<8>[10], vF : Fixed<8><<4>>[10]}}
    
    clock is invalid
    reset is invalid
    io is invalid
    wire lutGen : Fixed<8><<4>>[10] @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen is invalid @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[0] <= asFixedPoint(UInt<8>("h0cb"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[1] <= asFixedPoint(UInt<8>("h0dd"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[2] <= asFixedPoint(UInt<8>("h0ee"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[3] <= asFixedPoint(UInt<8>("h0f7"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[4] <= asFixedPoint(UInt<8>("h0fa"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[5] <= asFixedPoint(UInt<8>("h06"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[6] <= asFixedPoint(UInt<8>("h09"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[7] <= asFixedPoint(UInt<8>("h012"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[8] <= asFixedPoint(UInt<8>("h023"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    lutGen[9] <= asFixedPoint(UInt<8>("h035"), 4) @[SimpleTBwGenTypeOption.scala 170:19]
    wire lutS : SInt<8>[10] @[SimpleTBwGenTypeOption.scala 171:17]
    lutS is invalid @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[0] <= asSInt(UInt<8>("h0fd")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[1] <= asSInt(UInt<8>("h0fe")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[2] <= asSInt(UInt<8>("h0ff")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[3] <= asSInt(UInt<8>("h0ff")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[4] <= asSInt(UInt<8>("h00")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[5] <= asSInt(UInt<8>("h00")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[6] <= asSInt(UInt<8>("h01")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[7] <= asSInt(UInt<8>("h01")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[8] <= asSInt(UInt<8>("h02")) @[SimpleTBwGenTypeOption.scala 171:17]
    lutS[9] <= asSInt(UInt<8>("h03")) @[SimpleTBwGenTypeOption.scala 171:17]
    node _T_314 = bits(io.i.short.u, 3, 0)
    io.o.short.gen <= lutGen[_T_314] @[SimpleTBwGenTypeOption.scala 173:18]
    node _T_316 = bits(io.i.short.u, 3, 0)
    io.o.short.s <= lutS[_T_316] @[SimpleTBwGenTypeOption.scala 174:16]
    
