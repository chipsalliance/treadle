// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import treadle.executable.{ClockInfo, StopException}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

//scalastyle:off magic.number
class RiscVMiniSimpleSpec extends AnyFreeSpec with Matchers {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input =
      scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(input),
        ClockInfoAnnotation(
          Seq(ClockInfo("clock", period = 10, initialOffset = 1))
        )
      )
    )

    intercept[StopException] {
      tester.step(400)
    }
    tester.report()
    tester.engine.lastStopResult should be(Some(0))
  }
}
