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
      treadleOptions = TreadleOptions(setVerbose = true)
    }
    val engine = ExecutionEngine(input, optionsManager)

    engine.setValue("reset1", 1)
    engine.cycle()
    engine.getValue("reg1") should be (3)

    engine.setValue("reset1", 0)
    engine.cycle()
    engine.getValue("reg1") should be (4)

    engine.getValue("reg1") should be (4)

    engine.setValue("reset1", 0)
    engine.cycle()
    engine.getValue("reg1") should be (5)

    engine.setValue("reset1", 1)
    engine.cycle()
    engine.getValue("reg1") should be (3)

    engine.setValue("reset1", 0)
    engine.cycle()
    engine.getValue("reg1") should be (4)
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

    val engine = ExecutionEngine(input)

    // engine.setVerbose(true)
    engine.setValue("reset1", 1)
    engine.setValue("reset2", 0)
    engine.doCycles(1)
    engine.getValue("reg1") should be (0)

    engine.setValue("reset1", 0)
    engine.setValue("reset2", 1)
    engine.doCycles(1)
    engine.getValue("reg1") should be (1)
    engine.getValue("reg2") should be (0)

    engine.setValue("reset1", 1)
    engine.setValue("reset2", 1)
    engine.doCycles(1)
    engine.getValue("reg1") should be (0)
    engine.getValue("reg2") should be (1)

    engine.setValue("reset1", 0)
    engine.setValue("reset2", 0)
    engine.doCycles(1)
    engine.getValue("reg1") should be (1)
    engine.getValue("reg2") should be (4)

    engine.setValue("reset1", 0)
    engine.setValue("reset2", 0)
    engine.doCycles(1)
    engine.getValue("reg1") should be (2)
    engine.getValue("reg2") should be (7)

    engine.setValue("reset1", 1)
    engine.setValue("reset2", 0)
    engine.doCycles(1)
    engine.getValue("reg1") should be (0)
    engine.getValue("reg2") should be (10)

    engine.setValue("reset1", 0)
    engine.setValue("reset2", 0)
    engine.doCycles(1)
    engine.getValue("reg1") should be (1)
    engine.getValue("reg2") should be (13)

    engine.setValue("reset1", 0)
    engine.setValue("reset2", 1)
    engine.doCycles(1)
    engine.getValue("reg1") should be (2)
    engine.getValue("reg2") should be (1)

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
      treadleOptions = TreadleOptions(setVerbose = true)
    }
    val engine = ExecutionEngine(input, optionsManager)

    engine.setValue("reset1", 1)
    engine.cycle()
    engine.getValue("reg1") should be (3)
    engine.cycle()
    engine.getValue("reg1") should be (3)
    engine.cycle()
    engine.getValue("reg1") should be (3)

    engine.setValue("reset1", 0)
    engine.cycle()
    engine.getValue("reg1") should be (4)

    engine.getValue("reg1") should be (4)

    engine.setValue("reset1", 0)
    engine.cycle()
    engine.getValue("reg1") should be (5)

    engine.setValue("reset1", 1)
    engine.getValue("reg1") should be (5)
    engine.cycle()
    engine.getValue("reg1") should be (3)
    engine.cycle()
    engine.getValue("reg1") should be (3)

    engine.setValue("reset1", 0)
    engine.getValue("reg1") should be (3)
    engine.cycle()
    engine.getValue("reg1") should be (4)
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

    val tester = new TreadleTester(input)

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
