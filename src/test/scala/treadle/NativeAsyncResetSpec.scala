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
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

//scalastyle:off magic.number
class NativeAsyncResetSpec extends AnyFreeSpec with Matchers {
  "native firrtl async reset should work" in {
    val firrtlSource =
      """
        |circuit SimpleCircuit :
        |  module SimpleCircuit :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in : UInt<8>
        |    output out_reg : UInt<8>
        |    output out_async_reg : UInt<8>
        |
        |    wire a_reset : AsyncReset
        |    a_reset <= asAsyncReset(reset)
        |
        |    reg reg      : UInt<8>, clock with : (reset => (reset, UInt(17)))
        |    reg asyncReg : UInt<8>, clock with : (reset => (a_reset, UInt(17)))
        |
        |    reg <= in
        |    asyncReg <= in
        |    out_reg <= reg
        |    out_async_reg <= asyncReg
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      tester.poke("in", 7)
      tester.expect("out_reg", 0)
      tester.expect("out_async_reg", 0)

      tester.step()

      tester.expect("out_reg", 7)
      tester.expect("out_async_reg", 7)

      tester.poke("in", 23)
      tester.poke("reset", 1)

      tester.expect("reg", 7)
      tester.expect("out_async_reg", 17)

      tester.step()

      tester.expect("reg", 17)
      tester.expect("out_async_reg", 17)

      tester.poke("reset", 0)

      tester.expect("out_reg", 17)
      tester.expect("out_async_reg", 17)

      tester.step()

      tester.expect("out_reg", 23)
      tester.expect("out_async_reg", 23)
    }
  }
  "async reset should work when declared as IO" in {
    val firrtlSource =
      """
        |circuit SimpleCircuit :
        |  module SimpleCircuit :
        |    input clock : Clock
        |
        |    input a_reset : AsyncReset
        |    input reset : UInt<1>
        |
        |    input in : UInt<8>
        |    output out_reg : UInt<8>
        |    output out_async_reg : UInt<8>
        |
        |    reg reg      : UInt<8>, clock with : (reset => (reset, UInt(17)))
        |    reg asyncReg : UInt<8>, clock with : (reset => (a_reset, UInt(17)))
        |
        |    reg <= in
        |    asyncReg <= in
        |    out_reg <= reg
        |    out_async_reg <= asyncReg
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      tester.poke("in", 7)
      tester.expect("out_reg", 0)
      tester.expect("out_async_reg", 0)

      tester.step()

      tester.expect("out_reg", 7)
      tester.expect("out_async_reg", 7)

      tester.poke("in", 23)
      tester.poke("reset", 1)
      tester.poke("a_reset", 1)

      tester.expect("reg", 7)
      tester.expect("out_async_reg", 17)

      tester.step()

      tester.expect("reg", 17)
      tester.expect("out_async_reg", 17)

      tester.poke("reset", 0)
      tester.poke("a_reset", 0)

      tester.expect("out_reg", 17)
      tester.expect("out_async_reg", 17)

      tester.step()

      tester.expect("out_reg", 23)
      tester.expect("out_async_reg", 23)
    }
  }
}
