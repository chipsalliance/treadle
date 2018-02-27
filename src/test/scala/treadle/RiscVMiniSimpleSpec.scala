// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}

class RiscVMiniSimpleSpec extends FreeSpec with Matchers {
  "riscv-mini simple core test should run then stop" in {

    val input = io.Source.fromFile("samples/core-simple.lo.fir").getLines().mkString("\n")

    val optionsManager = new InterpreterOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = false,
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        symbolsToWatch = Seq("dut.io_host_tohost", "dut.dpath.csr.mtohost")
      )
    }

    val tester = new TreadleTester(input, optionsManager)

    intercept[StopException] {
      tester.step(300)
    }
    tester.engine.lastStopResult should be (Some(0))
  }
}

