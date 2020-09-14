// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FailSpec extends AnyFlatSpec with Matchers {
  behavior.of("explict fail")

  it should "fail a test with an explicit failure code" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.fail(3)
    tester.report()
    tester.reportString should include("Failed: Code 3")
  }
}
