// See LICENSE for license details.
package treadle

import firrtl.ExecutionOptionsManager
import org.scalatest.{FlatSpec, Matchers}

class LifeCellSpec extends FlatSpec with Matchers {
  behavior of "A Conway Life cell"

  it should "observe neighbor transition rules" in {
    val input =
      """
        |circuit LifeCell :
        |  module LifeCell :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip running : UInt<1>, flip top_left : UInt<4>, flip top_center : UInt<4>, flip top_right : UInt<4>, flip mid_left : UInt<4>, flip mid_right : UInt<4>, flip bot_left : UInt<4>, flip bot_center : UInt<4>, flip bot_right : UInt<4>, flip set_alive : UInt<1>, flip set_dead : UInt<1>, is_alive : UInt<1>}
        |
        |    io is invalid
        |    reg is_alive : UInt<1>, clock
        |    node T_13 = add(io.top_left, io.top_center)
        |    node sum0 = tail(T_13, 1)
        |    node T_14 = add(io.top_right, io.mid_left)
        |    node sum1 = tail(T_14, 1)
        |    node T_15 = add(io.mid_right, io.bot_left)
        |    node sum2 = tail(T_15, 1)
        |    node T_16 = add(io.bot_center, io.bot_right)
        |    node sum3 = tail(T_16, 1)
        |    node T_17 = add(sum0, sum1)
        |    node sum4 = tail(T_17, 1)
        |    node T_18 = add(sum2, sum3)
        |    node sum5 = tail(T_18, 1)
        |    node T_19 = add(sum4, sum5)
        |    node neighbor_sum = tail(T_19, 1)
        |    node T_22 = and(io.running, is_alive)
        |    node T_24 = eq(neighbor_sum, UInt<2>("h02"))
        |    node T_26 = eq(neighbor_sum, UInt<2>("h03"))
        |    node T_27 = or(T_24, T_26)
        |    node T_29 = eq(is_alive, UInt<1>("h00"))
        |    node T_30 = and(io.running, T_29)
        |    node T_32 = eq(neighbor_sum, UInt<2>("h03"))
        |    node T_33 = mux(T_30, T_32, is_alive)
        |    node T_34 = mux(T_22, T_27, T_33)
        |    node T_35 = mux(io.set_dead, UInt<1>("h00"), T_34)
        |    node T_36 = mux(io.set_alive, UInt<1>("h01"), T_35)
        |    is_alive <= T_36
        |    io.is_alive <= T_36
        |      """.stripMargin

    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--fint-write-vcd",
            "--firrtl-source", input,
            "--target-dir", "test_run_dir")) with HasTreadleSuite

    new TreadleTester(optionsManager) {
      // setVerbose()
      step()

      def setAlive(alive: Boolean): Unit = {
        poke("reset", 1)
        step()
        poke("reset", 0)
        poke("io_running", 0)
        poke("io_set_alive", if(alive) 1 else 0)
        poke("io_set_dead",  if(alive) 0 else 1)
        step()
        poke("io_running", 1)
        poke("io_set_alive", 0)
        poke("io_set_dead",  0)
      }

      // scalastyle:off parameter.number
      def setNeighborsIgnoreCenter(
                         ntl: Int, ntc: Int, ntr: Int,
                         nml: Int, nmc: Int, nmr: Int,
                         nbl: Int, nbc: Int, nbr: Int): Unit = {
        // center "neighbor" is the value of the cell itself
        //        poke("io_set_alive", nmc)
        poke("io_top_left", ntl)
        poke("io_top_center", ntc)
        poke("io_top_right", ntr)
        poke("io_mid_left", nml)

        poke("io_mid_right", nmr)
        poke("io_bot_left", nbl)
        poke("io_bot_center", nbc)
        poke("io_bot_right", nbr)
      }
      // scalastyle:on parameter.number

      setAlive(true)
      // dead cell with no neighbors stays dead
      setNeighborsIgnoreCenter(
        0,0,0,
        0,0,0,
        0,0,0
      )
      step()
      expect("io_is_alive", 0)

      // dead cell with > 3 neighbors stays dead
      setNeighborsIgnoreCenter(
        1,1,1,
        1,0,1,
        1,1,1
      )
      step()
      expect("io_is_alive", 0)

      // live cell with > 3 neighbors stays dead
      setNeighborsIgnoreCenter(
        1,1,1,
        1,1,1,
        1,1,1
      )
      step()
      expect("io_is_alive", 0)

      // dead cell with exactly three neighbors becomes alive
      setAlive(false)
      setNeighborsIgnoreCenter(
        1,0,0,
        1,0,0,
        1,0,0
      )
      step()
      expect("io_is_alive", 1)
      setNeighborsIgnoreCenter(
        1,0,0,
        0,0,1,
        0,1,0
      )
      step()
      expect("io_is_alive", 1)

      // live cell with one neighbor dies
      setNeighborsIgnoreCenter(
        0,0,0,
        0,1,1,
        0,0,0
      )
      step()
      expect("io_is_alive", 0)

      // live cell with exactly three neighbors stays alive
      setNeighborsIgnoreCenter(
        1,0,0,
        1,1,0,
        1,0,0
      )
      step()
      expect("io_is_alive", 1)

      // live cell with exactly four neighbors dies
      setNeighborsIgnoreCenter(
        1,0,0,
        1,1,1,
        1,0,0
      )
      step()
      expect("io_is_alive", 0)

      // test set_alive
      setNeighborsIgnoreCenter(
        0,0,0,
        0,0,0,
        0,0,0
      )

      step()
      poke("io_set_alive", 1)
      poke("io_set_dead", 0)
      poke("io_running", 1)
      step()
      expect("io_is_alive", 1)

      poke("io_set_alive", 1)
      poke("io_set_dead", 0)
      poke("io_running", 0)
      step()
      expect("io_is_alive", 1)

      poke("io_set_dead", 1)
      poke("io_set_alive", 0)
      poke("io_running", 1)
      step()
      expect("io_is_alive", 0)

      poke("io_set_dead", 1)
      poke("io_set_alive", 0)
      poke("io_running", 0)
      step()
      expect("io_is_alive", 0)

//      engine.circuitState.vcdLoggerOption.get.write(optionsManager.targetDirName + "/" + "life.vcd")
      report()
    }
  }
}
