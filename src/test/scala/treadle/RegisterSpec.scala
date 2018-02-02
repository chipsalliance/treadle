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
        |    reg1 <= mux(reset1, UInt<16>("h03"), add(reg1, UInt(1)))
        |
      """.stripMargin

    val optionsManager = new InterpreterOptionsManager {
      treadleOptions = treadleOptions.copy(setVerbose = false)
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

  it should "have register decrement as reset lowers" in {
    val input =
      """
        |circuit RegInc :
        |  module RegInc :
        |    input clock : Clock
        |    input reset : UInt<1>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt(3)))  @[RegisterSpec.scala 131:20]
        |
        |    reg1 <= add(reg1, UInt(1))
        |
      """.stripMargin

    val optionsManager = new InterpreterOptionsManager {
      treadleOptions = treadleOptions.copy(setVerbose = true, writeVCD = true)
    }
    val tester = new TreadleTester(input, optionsManager)
    tester.poke("reset", 1)
    tester.step()
    tester.poke("reset", 0)
    tester.step(1)
    tester.finish
  }

  behavior of "reset support, 2"

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
      treadleOptions = treadleOptions.copy(setVerbose = true, writeVCD = true)
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

  behavior of "multi-clock registers"

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

    val manager = new InterpreterOptionsManager {
      treadleOptions = treadleOptions.copy(
        showFirrtlAtLoad = false,
        setVerbose = false,
        rollbackBuffers = 15)
    }
    val tester = new TreadleTester(input, manager)

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
}
