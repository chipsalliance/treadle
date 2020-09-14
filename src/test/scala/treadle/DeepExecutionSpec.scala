// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class DeepExecutionSpec extends AnyFreeSpec with Matchers {
  "DeepExecutionSpec should pass a basic test" in {
    val stream = getClass.getResourceAsStream("/DeepExecution.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.step(100)
    tester.report()
  }
}
