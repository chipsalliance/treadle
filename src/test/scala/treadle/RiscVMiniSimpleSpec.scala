// See LICENSE for license details.

package treadle

import firrtl.ExecutionOptionsManager
import org.scalatest.{FreeSpec, Matchers}

class RiscVMiniSimpleSpec extends FreeSpec with Matchers {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val optionsManager = new ExecutionOptionsManager(
      "test",
      Array("--fint-write-vcd",
            "--fint-rollback-buffers", "0",
            "--firrtl-source", input)) with HasTreadleSuite
    val tester = new TreadleTester(optionsManager)

    intercept[StopException] {
      tester.step(300)
    }
    tester.report()
    tester.engine.lastStopResult should be (Some(0))
  }
}
