// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.{ClockInfo, StopException}

//scalastyle:off magic.number
class RiscVMiniSimpleSpec extends FreeSpec with Matchers {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = false,
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        rollbackBuffers = 0,
        clockInfo = Seq(ClockInfo("clock", period = 10, initialOffset = 1)),
        symbolsToWatch = Seq()
      )
    }

    val tester = TreadleTester(input, optionsManager)

    intercept[StopException] {
      tester.step(400)
    }
    tester.report()
    tester.engine.writeVCD()
    tester.engine.lastStopResult should be (Some(0))
  }
}

