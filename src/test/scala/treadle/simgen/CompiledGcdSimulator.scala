/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.simgen

import treadle.Regression.computeGcd

object CompiledGcdSimulator {
  //scalastyle:off method.length
  def main(args: Array[String]): Unit = {
    val width = 20

    val text =
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
         |    node GEN_0 = mux(_T_13, _T_15, x)
         |    x <= mux(io_e, io_a, GEN_0)
         |    node GEN_1 = mux(_T_17, _T_19, y)
         |    y <= mux(io_e, io_b, GEN_1)
         |    io_z <= x
         |    io_v <= _T_21
    """
        .stripMargin

    for (_ <- 0 until 1) {
      val tester = FastTester(text, args)

      val values =
        for {x <- 1 to 1000
             y <- 1 to 1000
             } yield (x, y, computeGcd(x, y)._1)

      val startTime = System.nanoTime()
      // tester.poke("clock", 1)

      for ((x, y, z) <- values) {
        tester.step(1)
        tester.poke("io_a", x)
        tester.poke("io_b", y)
        tester.poke("io_e", 1)
        tester.step(1)

        tester.poke("io_e", 0)
        tester.step(1)

        while (tester.peek("io_v") != BigInt(1)) {
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
}
