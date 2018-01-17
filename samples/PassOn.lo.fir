circuit PassOn : @[:@2.0]
  module PassOn : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_in : UInt<64> @[:@6.4]
    output io_out : UInt<64> @[:@6.4]
  
    skip
    io_out <= io_in
