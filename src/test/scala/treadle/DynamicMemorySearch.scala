// See LICENSE for license details.
package treadle

import firrtl.ExecutionOptionsManager
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by chick on 5/4/16.
  */
class DynamicMemorySearch extends FlatSpec with Matchers {
  behavior of "dynamic memory search"

  it should "run with correct results" in {
    val input =
    """
      |circuit DynamicMemorySearch :
      |  module DynamicMemorySearch :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<6>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
      |
      |    io is invalid
      |    reg index : UInt<3>, clk with : (reset => (reset, UInt<3>("h00")))
      |    cmem list : UInt<6>[8]
      |    infer mport memVal = list[index], clk
      |    node T_10 = eq(io.en, UInt<1>("h00"))
      |    node T_11 = eq(memVal, io.data)
      |    node T_13 = eq(index, UInt<3>("h07"))
      |    node T_14 = or(T_11, T_13)
      |    node over = and(T_10, T_14)
      |    when io.isWr :
      |      infer mport T_15 = list[io.wrAddr], clk
      |      T_15 <= io.data
      |      skip
      |    node T_17 = eq(io.isWr, UInt<1>("h00"))
      |    node T_18 = and(T_17, io.en)
      |    when T_18 :
      |      index <= UInt<1>("h00")
      |      skip
      |    node T_21 = eq(over, UInt<1>("h00"))
      |    node T_23 = eq(io.isWr, UInt<1>("h00"))
      |    node T_25 = eq(io.en, UInt<1>("h00"))
      |    node T_26 = and(T_23, T_25)
      |    node T_27 = and(T_26, T_21)
      |    when T_27 :
      |      node T_29 = add(index, UInt<1>("h01"))
      |      node T_30 = tail(T_29, 1)
      |      index <= T_30
      |      skip
      |    io.done <= over
      |    io.target <= index
    """.stripMargin

    val n = 8
    val w = 4

    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--firrtl-source", input)) with HasTreadleSuite

    new TreadleTester(optionsManager) {
//      engine.setVerbose(true)
//      engine.sourceState.memories("list").setVerbose()

      val list: Array[Int] = Array.fill(n)(0)
      random.setSeed(0L)

      // initialize memory
      for(write_address <- 0 until n) {
        println(s"Initializing memory address $write_address")
        poke("io_en", 0)
        poke("io_isWr", 1)
        poke("io_wrAddr", write_address)
        poke("io_data",   write_address)
        list(write_address) = write_address
        step()
      }

      // println(s"${engine.circuitState.memories("list").toString}")

      for (k <- 0 until 160) {
        println(s"memory test iteration $k") // ${"X"*80}")

        // Compute a random address and value
        val wrAddr = random.nextInt(n - 1)
        val data   = random.nextInt((1 << w) - 1)
        println(s"setting memory($wrAddr) = $data")

        // poke it intro memory
        poke("io_en", 0)
        poke("io_isWr", 1)
        poke("io_wrAddr", wrAddr)
        poke("io_data",   data)
        list(wrAddr) = data
        step()

        // SETUP SEARCH
        val target = if (k > 12) random.nextInt(1 << w) else data
        poke("io_isWr", 0)
        poke("io_data", target)
        poke("io_en", 1)
        step()
        poke("io_en", 0)
        val expectedIndex = if (list.contains(target)) {
          list.indexOf(target)
        } else {
          list.length - 1
        }
        // println(s"test pass $k ${this.engine.circuitState.prettyString()}")

        var waitCount = 0
        while(waitCount <= n && peek("io_done") == Big0) {
          // println(s"Waiting for done $waitCount")
          // println(this.engine.circuitState.prettyString())
          // println(s"external list ${list.mkString(",")}")
          step()
          waitCount += 1
        }

        // println(s"Done wait count is $waitCount done is ${peek("io_done")} " +
        //   s"got ${peek("io_target")} got $expectedIndex")
        expect("io_done", 1)
        expect("io_target", expectedIndex)
        step()

      }
      report()
    }
  }
}
