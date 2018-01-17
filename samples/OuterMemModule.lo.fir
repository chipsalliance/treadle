circuit OuterMemModule : @[:@2.0]
  module InnerMemModule : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
  
    mem nelly : @[MemPokeSpec.scala 13:18:@11.4]
      data-type => UInt<32>
      depth => 1024
      read-latency => 0
      write-latency => 1
      read-under-write => undefined

  module OuterMemModule : @[:@13.2]
    input clock : Clock @[:@14.4]
    input reset : UInt<1> @[:@15.4]
    input io_readAddress : UInt<10> @[:@16.4]
    output io_readData : UInt<32> @[:@16.4]
  
    mem billy : @[MemPokeSpec.scala 22:18:@21.4]
      data-type => UInt<32>
      depth => 1024
      read-latency => 0
      write-latency => 1
      reader => _T_6
      read-under-write => undefined
    inst inner of InnerMemModule @[MemPokeSpec.scala 23:21:@22.4]
    io_readData <= billy._T_6.data
    billy._T_6.addr <= io_readAddress @[:@13.2]
    billy._T_6.en <= UInt<1>("h1") @[:@13.2]
    billy._T_6.clk <= clock @[:@13.2]
    inner.clock <= clock
    inner.reset <= reset
