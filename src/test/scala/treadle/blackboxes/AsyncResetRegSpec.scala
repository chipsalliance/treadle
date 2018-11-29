// See LICENSE for license details.

package treadle.blackboxes

import org.scalatest.{FreeSpec, Matchers}
import treadle.asyncreset.AsyncResetBlackBoxFactory
import treadle.{TreadleOptionsManager, TreadleTester}

// scalastyle:off magic.number
class AsyncResetRegSpec extends FreeSpec with Matchers {
  "an async reset reg should behave like a normal reg using clock" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesAsyncResetReg :
        |  extmodule AsyncResetReg :
        |    input d : UInt<1>
        |    output q : UInt<1>
        |    input en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 0
        |
        |  extmodule AsyncResetReg_1 :
        |    input d : UInt<1>
        |    output q : UInt<1>
        |    input en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  extmodule AsyncResetReg_2 :
        |    input d : UInt<1>
        |    output q : UInt<1>
        |    input en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  module AsyncResetRegVec_w3_i42 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip d : UInt<3>, q : UInt<3>, flip en : UInt<1>}
        |
        |    inst reg_0 of AsyncResetReg @[AsyncResetRegTest.scala 45:11]
        |    reg_0.rst is invalid
        |    reg_0.clk is invalid
        |    reg_0.en is invalid
        |    reg_0.q is invalid
        |    reg_0.d is invalid
        |    inst reg_1 of AsyncResetReg_1 @[AsyncResetRegTest.scala 45:11]
        |    reg_1.rst is invalid
        |    reg_1.clk is invalid
        |    reg_1.en is invalid
        |    reg_1.q is invalid
        |    reg_1.d is invalid
        |    inst reg_2 of AsyncResetReg_2 @[AsyncResetRegTest.scala 45:11]
        |    reg_2.rst is invalid
        |    reg_2.clk is invalid
        |    reg_2.en is invalid
        |    reg_2.q is invalid
        |    reg_2.d is invalid
        |    reg_0.clk <= clock @[AsyncResetRegTest.scala 49:16]
        |    reg_0.rst <= reset @[AsyncResetRegTest.scala 50:16]
        |    node _T = bits(io.d, 0, 0) @[AsyncResetRegTest.scala 51:23]
        |    reg_0.d <= _T @[AsyncResetRegTest.scala 51:16]
        |    reg_0.en <= io.en @[AsyncResetRegTest.scala 52:16]
        |    reg_1.clk <= clock @[AsyncResetRegTest.scala 49:16]
        |    reg_1.rst <= reset @[AsyncResetRegTest.scala 50:16]
        |    node _T_1 = bits(io.d, 1, 1) @[AsyncResetRegTest.scala 51:23]
        |    reg_1.d <= _T_1 @[AsyncResetRegTest.scala 51:16]
        |    reg_1.en <= io.en @[AsyncResetRegTest.scala 52:16]
        |    reg_2.clk <= clock @[AsyncResetRegTest.scala 49:16]
        |    reg_2.rst <= reset @[AsyncResetRegTest.scala 50:16]
        |    node _T_2 = bits(io.d, 2, 2) @[AsyncResetRegTest.scala 51:23]
        |    reg_2.d <= _T_2 @[AsyncResetRegTest.scala 51:16]
        |    reg_2.en <= io.en @[AsyncResetRegTest.scala 52:16]
        |    node _T_3 = cat(reg_0.q, reg_1.q) @[Cat.scala 30:58]
        |    node _T_4 = cat(_T_3, reg_2.q) @[Cat.scala 30:58]
        |    io.q <= _T_4 @[AsyncResetRegTest.scala 57:8]
        |
        |  module UsesAsyncResetReg :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<3>, out : UInt<3>}
        |
        |    inst reg of AsyncResetRegVec_w3_i42 @[AsyncResetRegTest.scala 69:19]
        |    reg.clock <= clock
        |    reg.reset <= reset
        |    reg.io.d <= io.in @[AsyncResetRegTest.scala 71:12]
        |    reg.clock <= clock @[AsyncResetRegTest.scala 72:13]
        |    reg.reset <= reset @[AsyncResetRegTest.scala 73:13]
        |    reg.io.en <= UInt<1>("h01") @[AsyncResetRegTest.scala 74:13]
        |    io.out <= reg.io.q @[AsyncResetRegTest.scala 76:10]
        |
        |
    """.stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        blackBoxFactories = Seq(new AsyncResetBlackBoxFactory),
        callResetAtStartUp = false,
        setVerbose = true
      )
    }
    val tester = TreadleTester(input, manager)

    // poke a value and it should not appear as reg output until after step
    tester.poke("io_in", 7)
    tester.expect("io_out", 0)

    tester.step()

    tester.expect("io_out", 7)

    // reset should make immediate change to register
    tester.poke("reset", 1)
    tester.expect("io_out", 3)

    tester.step()

    tester.expect("io_out", 3)
  }

  "single bit example of an async reset reg should behave like a normal reg using clock" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesAsyncResetReg :
        |  extmodule AsyncResetReg :
        |    input d : UInt<1>
        |    output q : UInt<1>
        |    input en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  module UsesAsyncResetReg :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<1>, flip en : UInt<1>, out : UInt<1>}
        |
        |    inst reg_0 of AsyncResetReg @[AsyncResetRegTest.scala 45:11]
        |    reg_0.rst is invalid
        |    reg_0.clk is invalid
        |    reg_0.en is invalid
        |    reg_0.q is invalid
        |    reg_0.d is invalid
        |
        |    reg_0.clk <= clock
        |    reg_0.rst <= reset
        |    reg_0.d <= io.in  @[AsyncResetRegTest.scala 51:16]
        |    reg_0.en <= io.en @[AsyncResetRegTest.scala 52:16]
        |
        |    io.out <= reg_0.q
        |
        |
    """.stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        blackBoxFactories = Seq(new AsyncResetBlackBoxFactory),
        callResetAtStartUp = false,
        setVerbose = true
      )
    }
    val tester = TreadleTester(input, manager)

    // test that setting the input is only seen after step
    tester.poke("io_in", 1)
    tester.poke("io_en", 1)
    tester.expect("io_out", 0)

    tester.step()

    tester.expect("io_out", 1)

    // test that enable defeats register update on step
    tester.poke("io_in", 0)
    tester.poke("io_en", 0)
    tester.expect("io_out", 1)

    tester.step()

    tester.expect("io_out", 1)

    // setting enable lets register update on step again
    tester.poke("io_en", 1)
    tester.expect("io_out", 1)

    tester.step()

    tester.expect("io_out", 0)

    // setting reset immediately updates register
    tester.poke("reset", 1)
    tester.expect("io_out", 1)

    tester.step()

    tester.expect("io_out", 1)
  }
}
