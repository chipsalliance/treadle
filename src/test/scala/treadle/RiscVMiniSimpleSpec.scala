// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}

class RiscVMiniSimpleSpec extends FreeSpec with Matchers {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = true,
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        rollbackBuffers = 0,
        symbolsToWatch = Seq() // Seq("dut.io_host_tohost", "dut.dpath.csr.mtohost")
      )
    }

    val tester = new TreadleTester(input, optionsManager)

//    intercept[StopException] {
      tester.step(300)
//    }
    tester.report()
    tester.engine.writeVCD()
    tester.engine.lastStopResult should be (Some(0))
  }
}

