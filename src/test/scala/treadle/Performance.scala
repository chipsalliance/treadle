// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}

class Performance extends FreeSpec with Matchers {
  "how fast is this" in {
    val width = 32

    val junkFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    output io_z : UInt<$width>
         |
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), y)
         |
         |    x <= add(x, io_a)
         |    y <= add(y, io_b)
         |
         |    io_z <= add(x, y)
    """
              .stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        rollbackBuffers = 0, showFirrtlAtLoad = false, setVerbose = false, writeVCD = false)
    }

    val tester = new TreadleTester(junkFirrtl, manager)

    val startTime = System.nanoTime()

    for(i <- 0 to 30) {
      tester.poke("io_a", 1)
      tester.poke("io_b", 2)
      tester.step(1000000)
      println(s"trial $i got ${tester.peek("io_z")}")
    }

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    val cycle = tester.cycleCount

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()

  }

}
