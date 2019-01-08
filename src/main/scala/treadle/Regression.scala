// See LICENSE for license details.

package treadle

object Regression {
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while(y > 0 ) {
      if (x > y) {
        x -= y
      }
      else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  //scalastyle:off method.length
  def manyValuesTest(width: Int) {
    val gcdFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    input io_e : UInt<1>
         |    output io_z : UInt<$width>
         |    output io_v : UInt<1>
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), y)
         |    node _T_13 = gt(x, y)
         |    node _T_14 = sub(x, y)
         |    node _T_15 = tail(_T_14, 1)
         |    node _T_17 = eq(_T_13, UInt<1>("h0"))
         |    node _T_18 = sub(y, x)
         |    node _T_19 = tail(_T_18, 1)
         |    node _T_21 = eq(y, UInt<1>("h0"))
         |    node _GEN_0 = mux(_T_13, _T_15, x)
         |    x <= mux(io_e, io_a, _GEN_0)
         |    node _GEN_1 = mux(_T_17, _T_19, y)
         |    y <= mux(io_e, io_b, _GEN_1)
         |    io_z <= x
         |    io_v <= _T_21
    """
              .stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        showFirrtlAtLoad = false,
        setVerbose = false,
        rollbackBuffers = 0
      )
    }

    val values =
      for {x <- 1 to 1000
           y <- 1 to 1000
      } yield (x, y, computeGcd(x, y)._1)

    val tester = new TreadleTester(gcdFirrtl, manager)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    for((x, y, z) <- values) {
      tester.step()
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step()

      tester.poke("io_e", 0)
      tester.step()

      while (tester.peek("io_v") != Big1) {
        tester.step()
      }

      tester.expect("io_z", z)
    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    val cycle = tester.cycleCount

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()
  }




  def main(args: Array[String]): Unit = {
    manyValuesTest(20)
  }
}
