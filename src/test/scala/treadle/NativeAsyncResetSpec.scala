// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class NativeAsyncResetSpec extends FreeSpec with Matchers {
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

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        rollbackBuffers = 0, showFirrtlAtLoad = false, setVerbose = false, writeVCD = false)
    }
    val tester = TreadleTester(firrtlSource, manager)

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

    tester.report()
    tester.finish
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

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        rollbackBuffers = 0, showFirrtlAtLoad = false, setVerbose = false, writeVCD = false)
    }
    val tester = TreadleTester(firrtlSource, manager)

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

    tester.report()
    tester.finish
  }
}
