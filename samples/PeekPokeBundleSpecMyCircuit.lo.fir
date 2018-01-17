circuit PeekPokeBundleSpecMyCircuit : @[:@2.0]
  module PeekPokeBundleSpecMyCircuit : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in_aUInt4 : UInt<4> @[:@6.4]
    input io_in_aSInt5 : SInt<5> @[:@6.4]
    input io_in_aBundle_aBool : UInt<1> @[:@6.4]
    input io_in_aBottomBool : UInt<1> @[:@6.4]
    output io_out_aUInt4 : UInt<4> @[:@6.4]
    output io_out_aSInt5 : SInt<5> @[:@6.4]
    output io_out_aBundle_aBool : UInt<1> @[:@6.4]
    output io_out_aBottomBool : UInt<1> @[:@6.4]
  
    skip
    io_out_aUInt4 <= io_in_aUInt4
    io_out_aSInt5 <= io_in_aSInt5
    io_out_aBundle_aBool <= io_in_aBundle_aBool
    io_out_aBottomBool <= io_in_aBottomBool
