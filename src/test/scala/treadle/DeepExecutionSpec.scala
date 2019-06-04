// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class DeepExecutionSpec extends FreeSpec with Matchers {
  "DeepExecutionSpec should pass a basic test" in {
    val stream = getClass.getResourceAsStream("/DeepExecution.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val tester = TreadleTester(input)

    tester.step(100)
    tester.report()
  }
}
