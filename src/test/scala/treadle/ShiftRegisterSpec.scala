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
import treadle.executable.StopException

// scalastyle:off magic.number
class ShiftRegisterSpec extends FreeSpec with Matchers {
  "ShiftRegisterSpec should pass a basic test" in {
    val input =
      """
        |circuit ShiftResetTester :
        |  module ShiftResetTester :
        |    input clock : Clock
        |    input reset : UInt<1>
        |
        |    reg sr : UInt<5>, clock with :
        |      reset => (UInt<1>("h0"), sr) @[Reg.scala 19:20]
        |    node done = and(UInt<1>("h1"), UInt<1>("h1")) @[Counter.scala 64:20]
        |    node _T_5 = add(UInt<1>("h0"), UInt<5>("h17")) @[Reg.scala 58:33]
        |    node _T_6 = tail(_T_5, 1) @[Reg.scala 58:33]
        |    node _GEN_0 = mux(UInt<1>("h1"), _T_6, sr) @[Reg.scala 20:19]
        |    node _T_11 = eq(sr, UInt<1>("h1")) @[Reg.scala 60:15]
        |    node _T_12 = bits(reset, 0, 0) @[Reg.scala 60:11]
        |    node _T_13 = or(_T_11, _T_12) @[Reg.scala 60:11]
        |    node _T_15 = eq(_T_13, UInt<1>("h0")) @[Reg.scala 60:11]
        |    node _T_16 = bits(reset, 0, 0) @[Reg.scala 61:9]
        |    node _T_18 = eq(_T_16, UInt<1>("h0")) @[Reg.scala 61:9]
        |    sr <= mux(reset, UInt<1>("h1"), _GEN_0)
        |    printf(clock, and(and(and(UInt<1>("h1"), done), _T_15), UInt<1>("h1")), "Assertion failed\n    at Reg.scala:60 assert(sr === 1.U)\n") @[Reg.scala 60:11]
        |    stop(clock, and(and(and(UInt<1>("h1"), done), _T_15), UInt<1>("h1")), 1) @[Reg.scala 60:11]
        |    stop(clock, and(and(and(UInt<1>("h1"), done), _T_18), UInt<1>("h1")), 0) @[Reg.scala 61:9]
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), RollBackBuffersAnnotation(4)))

    intercept[StopException] {
      tester.step(8)
    }
    tester.engine.lastStopResult should be(Some(1))
    tester.report()
  }
}
