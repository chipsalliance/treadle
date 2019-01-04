// See LICENSE for license details.

package treadle

import treadle.Regression.computeGcd
import treadle.executable.SimplePokerPeeker

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object OnTheFly {
  def main(args: Array[String]): Unit = {
    val text = io.Source.fromFile("GCDScalaImpl.scala").getLines().mkString("\n") +
    "\nnew GCDScalaImpl\n"

    println(text)

    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

    println(s"\nparsing\n")
    val parsed = tb.eval(tb.parse(text))
    println(parsed)
    println(s"\n\nevaluating\n")

    val evald = tb.eval(tb.parse(text)).asInstanceOf[SimplePokerPeeker]

    val values =
      for {x <- 1 to 1000
           y <- 1 to 1000
      } yield (x, y, computeGcd(x, y)._1)

    val startTime = System.nanoTime()
    evald.poke("clock", 1)

    for((x, y, z) <- values) {
      evald.step(1)
      evald.poke("io_a", x)
      evald.poke("io_b", y)
      evald.poke("io_e", 1)
      evald.step(1)

      evald.poke("io_e", 0)
      evald.step(1)

      while (evald.peek("io_v") != Big1) {
        evald.step(1)
      }

      //    println(s"gcd($x, $y) => ${evald.peek("io_z")}, predicted => $z")
      assert(evald.peek("io_z") == z)

    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println(s"${evald.cycles} cycles in elapsed seconds $elapsedSeconds")
    println(s"${evald.cycles.toDouble / (elapsedSeconds * 1000.0)} kHz")

  }
}

object OTFGCD {
  //scalastyle:off method.length
  def main(args: Array[String]): Unit = {
    val width = 20

    val text = s"""
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
                  |    node GEN_0 = mux(_T_13, _T_15, x)
                  |    x <= mux(io_e, io_a, GEN_0)
                  |    node GEN_1 = mux(_T_17, _T_19, y)
                  |    y <= mux(io_e, io_b, GEN_1)
                  |    io_z <= x
                  |    io_v <= _T_21
    """
            .stripMargin

    val tester = FastTester(text, args)

    val values =
      for {x <- 1 to 1000
           y <- 1 to 1000
      } yield (x, y, computeGcd(x, y)._1)

    val startTime = System.nanoTime()
    // tester.poke("clock", 1)

    for((x, y, z) <- values) {
      tester.step(1)
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step(1)

      tester.poke("io_e", 0)
      tester.step(1)

      while (tester.peek("io_v") != Big1) {
        tester.step(1)
      }

      // println(s"gcd($x, $y) => ${tester.peek("io_z")}, predicted => $z")
      assert(tester.peek("io_z") == z)

    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println(s"${tester.cycles} cycles in elapsed seconds $elapsedSeconds")
    println(s"${tester.cycles.toDouble / (elapsedSeconds * 1000.0)} kHz")

  }
}
