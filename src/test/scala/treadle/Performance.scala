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

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

/**
  * This needs work, we should be running through different kinds of ops
  * muxes etc, and coming up with some sort of performance formula
  */
class Performance extends FreeSpec with Matchers {
  "how fast is this".ignore {
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
    """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(junkFirrtl)))

    val startTime = System.nanoTime()

    tester.poke("io_a", 1)
    tester.poke("io_b", 2)

    for (i <- 0 to 30) {
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
