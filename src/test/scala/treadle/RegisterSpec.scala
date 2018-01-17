// See LICENSE for license details.
package treadle

import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number
class RegisterSpec extends FlatSpec with Matchers {
  behavior of "register reset"

  it should "reset registers when there condition is true" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset1 : UInt<1>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))
        |
        |    reg1 <= add(reg1, UInt(1))
        |
      """.stripMargin

    val optionsManager = new InterpreterOptionsManager {
      interpreterOptions = InterpreterOptions(setVerbose = true)
    }
    val interpreter = FirrtlTerp(input, optionsManager)

    interpreter.setValue("reset1", 1)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)

    interpreter.setValue("reset1", 0)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (4)

    interpreter.getValue("reg1") should be (4)

    interpreter.setValue("reset1", 0)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (5)

    interpreter.setValue("reset1", 1)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)

    interpreter.setValue("reset1", 0)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (4)
  }

  it should "be able to initialize registers from other places" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input reset1 : UInt<1>
        |    input reset2 : UInt<1>
        |
        |    reg reg1 : UInt<16>, clk with : (reset => (reset1, UInt<16>(0)))
        |    reg reg2 : UInt<16>, clk with : (reset => (reset2, reg1))
        |
        |    reg1 <= add(reg1, UInt(1))
        |    reg2 <= add(reg2, UInt(3))
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    // interpreter.setVerbose(true)
    interpreter.setValue("reset1", 1)
    interpreter.setValue("reset2", 0)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (0)

    interpreter.setValue("reset1", 0)
    interpreter.setValue("reset2", 1)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (1)
    interpreter.getValue("reg2") should be (0)

    interpreter.setValue("reset1", 1)
    interpreter.setValue("reset2", 1)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (0)
    interpreter.getValue("reg2") should be (1)

    interpreter.setValue("reset1", 0)
    interpreter.setValue("reset2", 0)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (1)
    interpreter.getValue("reg2") should be (4)

    interpreter.setValue("reset1", 0)
    interpreter.setValue("reset2", 0)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (2)
    interpreter.getValue("reg2") should be (7)

    interpreter.setValue("reset1", 1)
    interpreter.setValue("reset2", 0)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (0)
    interpreter.getValue("reg2") should be (10)

    interpreter.setValue("reset1", 0)
    interpreter.setValue("reset2", 0)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (1)
    interpreter.getValue("reg2") should be (13)

    interpreter.setValue("reset1", 0)
    interpreter.setValue("reset2", 1)
    interpreter.doCycles(1)
    interpreter.getValue("reg1") should be (2)
    interpreter.getValue("reg2") should be (1)

  }

  behavior of "reset support"

  it should "reset takes precedence over next value" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset1 : UInt<1>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))  @[RegisterSpec.scala 131:20]
        |
        |    reg1 <= add(reg1, UInt(1))
        |
      """.stripMargin

    val optionsManager = new InterpreterOptionsManager {
      interpreterOptions = InterpreterOptions(setVerbose = true)
    }
    val interpreter = FirrtlTerp(input, optionsManager)

    interpreter.setValue("reset1", 1)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)

    interpreter.setValue("reset1", 0)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (4)

    interpreter.getValue("reg1") should be (4)

    interpreter.setValue("reset1", 0)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (5)

    interpreter.setValue("reset1", 1)
    interpreter.getValue("reg1") should be (5)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (3)

    interpreter.setValue("reset1", 0)
    interpreter.getValue("reg1") should be (3)
    interpreter.cycle()
    interpreter.getValue("reg1") should be (4)
  }

  behavior of "poking registers"

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

    val tester = new InterpretiveTester(input)

    tester.poke("in", 7)
    tester.step()
    tester.peek("reg1") should be (7)
    tester.poke("in", 3)
    tester.step()
    tester.peek("reg1") should be (3)

    tester.poke("in", 8)
    tester.poke("reg1", 42)
    tester.peek("reg1") should be (42)
    tester.peek("T_1") should be (42)
    tester.step()
    tester.peek("T_2") should be (42)
    tester.peek("reg2") should be (42)
    tester.peek("reg1") should be (8)
  }
}
