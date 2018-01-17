circuit TestMem : @[:@2.0]
  module TestMem : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
  
    mem mem : @[TestMem.scala 5:16:@11.4]
      data-type => UInt<32>
      depth => 1024
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
