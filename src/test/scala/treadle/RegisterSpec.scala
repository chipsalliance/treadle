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
import firrtl.transforms.NoDCEAnnotation
import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number
class RegisterSpec extends FlatSpec with Matchers {
  behavior.of("register reset")

  it should "reset registers when their condition is true" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset1 : UInt<1>
        |    output out1 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))
        |
        |    reg1 <= mux(reset1, UInt<16>("h03"), add(reg1, UInt(1)))
        |    out1 <= reg1
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation))

    tester.poke("reset1", 1)
    tester.step()
    tester.peek("reg1") should be(3)

    tester.poke("reset1", 0)
    tester.step()
    tester.peek("reg1") should be(4)

    tester.peek("reg1") should be(4)

    tester.poke("reset1", 0)
    tester.step()
    tester.peek("reg1") should be(5)

    tester.poke("reset1", 1)
    tester.step()
    tester.peek("reg1") should be(3)

    tester.poke("reset1", 0)
    tester.step()
    tester.peek("reg1") should be(4)
  }

  it should "be able to initialize registers from other places" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input reset1 : UInt<1>
        |    input reset2 : UInt<1>
        |    output out1 : UInt<16>
        |    output out2 : UInt<16>
        |
        |
        |    reg reg1 : UInt<16>, clk with : (reset => (reset1, UInt<16>(0)))
        |    reg reg2 : UInt<16>, clk with : (reset => (reset2, reg1))
        |
        |    reg1 <= add(reg1, UInt(1))
        |    reg2 <= add(reg2, UInt(3))
        |
        |    out1 <= reg1
        |    out2 <= reg2
        |
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation))

    tester.poke("reset1", 1)
    tester.poke("reset2", 0)
    tester.step()
    tester.peek("reg1") should be(0)

    tester.poke("reset1", 0)
    tester.poke("reset2", 1)
    tester.step()
    tester.peek("reg1") should be(1)
    tester.peek("reg2") should be(0)

    tester.poke("reset1", 1)
    tester.poke("reset2", 1)
    tester.step()
    tester.peek("reg1") should be(0)
    tester.peek("reg2") should be(1)

    tester.poke("reset1", 0)
    tester.poke("reset2", 0)
    tester.step()
    tester.peek("reg1") should be(1)
    tester.peek("reg2") should be(4)

    tester.poke("reset1", 0)
    tester.poke("reset2", 0)
    tester.step()
    tester.peek("reg1") should be(2)
    tester.peek("reg2") should be(7)

    tester.poke("reset1", 1)
    tester.poke("reset2", 0)
    tester.step()
    tester.peek("reg1") should be(0)
    tester.peek("reg2") should be(10)

    tester.poke("reset1", 0)
    tester.poke("reset2", 0)
    tester.step()
    tester.peek("reg1") should be(1)
    tester.peek("reg2") should be(13)

    tester.poke("reset1", 0)
    tester.poke("reset2", 1)
    tester.step()
    tester.peek("reg1") should be(2)
    tester.peek("reg2") should be(1)

  }

  behavior.of("reset support")

  it should "have register decrement as reset lowers" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<16>
        |
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt(3)))  @[RegisterSpec.scala 131:20]
        |
        |    reg1 <= add(reg1, UInt(1))
        |    out1 <= reg1
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.poke("reset", 1)
    tester.step()
    tester.poke("reset", 0)
    tester.step()
    tester.finish
  }

  behavior.of("reset support, 2")

  it should "reset takes precedence over next value" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset1 : UInt<1>
        |    output out1 : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))  @[RegisterSpec.scala 131:20]
        |
        |    reg1 <= add(reg1, UInt(1))
        |    out1 <= reg1
        |
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation))

    tester.poke("reset1", 1)
    tester.step()
    tester.peek("reg1") should be(3)
    tester.step()
    tester.peek("reg1") should be(3)
    tester.step()
    tester.peek("reg1") should be(3)

    tester.poke("reset1", 0)
    tester.step()
    tester.peek("reg1") should be(4)

    tester.peek("reg1") should be(4)

    tester.poke("reset1", 0)
    tester.step()
    tester.peek("reg1") should be(5)

    tester.poke("reset1", 1)
    tester.peek("reg1") should be(5)
    tester.step()
    tester.peek("reg1") should be(3)
    tester.step()
    tester.peek("reg1") should be(3)

    tester.poke("reset1", 0)
    tester.peek("reg1") should be(3)
    tester.step()
    tester.peek("reg1") should be(4)
  }

  behavior.of("poking registers")

  it should "poke a register" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input in : UInt<16>
        |    output out : UInt<16>
        |
        |    reg reg1 : UInt<16>, clk
        |    reg reg2 : UInt<16>, clk
        |    wire T_1 : UInt<16>
        |    wire T_2 : UInt<16>
        |
        |    reg1 <= in
        |    T_1 <= reg1
        |    reg2 <= T_1
        |    T_2 <= reg2
        |    out <= T_2
        |
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation))

    tester.poke("in", 7)
    tester.step()
    tester.peek("reg1") should be(7)
    tester.poke("in", 3)
    tester.step()
    tester.peek("reg1") should be(3)

    tester.poke("in", 8)
    tester.poke("reg1", 42)
    tester.peek("reg1") should be(42)
    tester.step()
    tester.peek("reg2") should be(42)
    tester.peek("reg1") should be(8)
  }

  behavior.of("multi-clock registers")

  it should "get the timing right" in {
    val input =
      """
        |circuit RegisterDependencies : @[:@2.0]
        |  module RegisterDependencies : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    input io_in : UInt<16> @[:@6.4]
        |    input io_en : UInt<1> @[:@6.4]
        |    output io_o1 : UInt<16> @[:@6.4]
        |    output io_o2 : UInt<16> @[:@6.4]
        |
        |    reg reg1 : UInt<16>, clock with :
        |      reset => (UInt<1>("h0"), reg1)
        |    reg clockToggle : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), clockToggle)
        |    reg reg2 : UInt<16>, clock with :
        |      reset => (UInt<1>("h0"), reg2)
        |
        |    node _T_8 = add(reg1, UInt<1>("h1")) @[RegisterDependencies.scala 17:16:@9.4]
        |    node _T_9 = tail(_T_8, 1) @[RegisterDependencies.scala 17:16:@10.4]
        |
        |    node _T_13 = eq(clockToggle, UInt<1>("h0")) @[RegisterDependencies.scala 20:18:@13.4]
        |    node _T_14 = and(io_en, clockToggle) @[RegisterDependencies.scala 22:23:@15.4]
        |    node clock2 = asClock(_T_14) @[RegisterDependencies.scala 22:39:@16.4]
        |
        |    node _T_18 = add(reg2, UInt<1>("h1")) @[RegisterDependencies.scala 26:18:@18.4]
        |    node _T_19 = tail(_T_18, 1) @[RegisterDependencies.scala 26:18:@19.4]
        |
        |    io_o1 <= reg1
        |    io_o2 <= reg2
        |
        |    reg1 <= mux(reset, io_in, _T_9)
        |    clockToggle <= mux(reset, UInt<1>("h1"), _T_13)
        |    reg2 <= mux(reset, UInt<7>("h33"), _T_19)
        |
      """.stripMargin

    val tester = TreadleTester(
      Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation, RollBackBuffersAnnotation(15))
    )

    tester.poke("io_in", 77)
    tester.poke("io_en", 0)
    tester.poke("reset", 1)
    tester.step()
    tester.expect("reg1/in", 77)
    tester.expect("reg2/in", 51)

    tester.poke("reset", 0)
    tester.step()
    tester.expect("reg1", 78)
    tester.expect("reg2", 52)

    tester.step()
    tester.expect("reg1", 79)
    tester.expect("reg2", 53)

    tester.poke("io_en", 1)
    tester.step()
    tester.expect("reg1", 80)
    tester.expect("reg2", 54)

  }

  behavior.of("mutually connected registers")

  it should "alternate values" in {
    val input =
      """
        |circuit AsyncResetRegTestanon6 :
        |  module AsyncResetRegTestanon6 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io_out_0 : UInt<1>
        |    output io_out_1 : UInt<1>
        |
        |    reg reg0 : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), reg0)
        |    reg reg1 : UInt<1>, clock with :
        |      reset => (UInt<1>("h0"), reg1)
        |
        |    io_out_0 <= reg0 @[AsyncResetRegTest.scala 162:16]
        |    io_out_1 <= reg1 @[AsyncResetRegTest.scala 163:16]
        |    reg0 <= mux(reset, UInt<1>("h0"), reg1) @[AsyncResetRegTest.scala 159:12]
        |    reg1 <= mux(reset, UInt<1>("h1"), reg0) @[AsyncResetRegTest.scala 160:12]
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), ShowFirrtlAtLoadAnnotation, NoDCEAnnotation))

    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 0)
    tester.step()
    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 0)

    tester.poke("reset", 1)
    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 0)

    tester.step()
    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 1)

    tester.poke("reset", 0)
    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 1)

    tester.step()
    tester.expect("io_out_0", 1)
    tester.expect("io_out_1", 0)

    tester.step()
    tester.expect("io_out_0", 0)
    tester.expect("io_out_1", 1)
  }
}
