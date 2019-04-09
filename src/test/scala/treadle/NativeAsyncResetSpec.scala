// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}

class NativeAsyncResetSpec extends FreeSpec with Matchers {
  "native firrtl async reset should work" in {
    val firrtlSource =
      """
        |circuit SimpleCircuit :
        |  module SimpleCircuit :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in : UInt<8>
        |    output out : UInt<8>
        |    wire areset : AsyncReset
        |    reg r : UInt<8>, clock with : (reset => (areset, UInt(0)))
        |    areset <= asAsyncReset(reset)
        |    r <= in
        |    out <= r
        |
      """.stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        rollbackBuffers = 0, showFirrtlAtLoad = true, setVerbose = true, writeVCD = false)
    }
    val tester = TreadleTester(firrtlSource, manager)

    tester.poke("in", 7)
    tester.expect("out", 0)

    tester.step()

    tester.poke("in", 23)
    tester.poke("reset", 1)
    tester.expect("out", 23)

    tester.report()
    tester.finish
  }
}
