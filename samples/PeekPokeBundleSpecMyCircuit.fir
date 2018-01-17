;buildInfoPackage: chisel3, version: 3.1-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-06-08 17:59:39.314, builtAtMillis: 1496944779314
circuit PeekPokeBundleSpecMyCircuit : 
  module PeekPokeBundleSpecMyCircuit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : {aUInt4 : UInt<4>, aSInt5 : SInt<5>, aBundle : {aBool : UInt<1>}, aBottomBool : UInt<1>}, out : {aUInt4 : UInt<4>, aSInt5 : SInt<5>, aBundle : {aBool : UInt<1>}, aBottomBool : UInt<1>}}
    
    clock is invalid
    reset is invalid
    io is invalid
    io.out.aBottomBool <= io.in.aBottomBool @[PeekPokeBundleSpec.scala 36:12]
    io.out.aBundle.aBool <= io.in.aBundle.aBool @[PeekPokeBundleSpec.scala 36:12]
    io.out.aSInt5 <= io.in.aSInt5 @[PeekPokeBundleSpec.scala 36:12]
    io.out.aUInt4 <= io.in.aUInt4 @[PeekPokeBundleSpec.scala 36:12]
    
