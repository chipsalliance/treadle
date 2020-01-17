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

package treadle.chronometry

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.TreadleTester

// scalastyle:off magic.number
class ClockMadnessSpec extends FreeSpec with Matchers {
  val input: String =
    """
      |circuit HasGatedCounter : @[:@2.0]
      |  module GatedCounter : @[:@3.2]
      |    input clock : Clock @[:@4.4]
      |    input reset : UInt<1> @[:@5.4]
      |    output io_count : UInt<32> @[:@6.4]
      |
      |    reg counter : UInt<32>, clock with :
      |      reset => (UInt<1>("h0"), counter) @[ClockMadness.scala 13:24:@8.4]
      |    node _T_6 = add(counter, UInt<1>("h1")) @[ClockMadness.scala 15:22:@9.4]
      |    node _T_7 = tail(_T_6, 1) @[ClockMadness.scala 15:22:@10.4]
      |    io_count <= counter
      |    counter <= mux(reset, UInt<32>("h0"), _T_7)
      |
      |  module HasGatedCounter : @[:@14.2]
      |    input clock : Clock @[:@15.4]
      |    input reset : UInt<1> @[:@16.4]
      |    input io_enable : UInt<1> @[:@17.4]
      |    output io_count : UInt<32> @[:@17.4]
      |
      |    node _T_4 = asUInt(clock) @[ClockMadness.scala 26:29:@19.4]
      |    node _T_5 = bits(_T_4, 0, 0) @[ClockMadness.scala 26:31:@20.4]
      |    node _T_6 = and(_T_5, io_enable) @[ClockMadness.scala 26:35:@21.4]
      |    node clock2 = asClock(_T_6) @[ClockMadness.scala 26:55:@22.4]
      |    inst GatedCounter of GatedCounter @[ClockMadness.scala 29:31:@23.4]
      |    io_count <= GatedCounter.io_count
      |    GatedCounter.clock <= clock2
      |    GatedCounter.reset <= reset
    """.stripMargin

  "ClockMadnessSpec should pass a basic test" in {

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
    tester.poke("io_enable", 0)
    for (_ <- 0 until 10) {
      println(s"counter = ${tester.peek("io_count")}")
      tester.step()
    }

    tester.poke("io_enable", 1)
    for (_ <- 0 until 10) {
      println(s"counter = ${tester.peek("io_count")}")
      tester.step()
    }
  }
}
