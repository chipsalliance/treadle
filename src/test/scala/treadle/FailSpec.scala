// See LICENSE for license details.

package treadle

import org.scalatest.{FlatSpec, Matchers}

class FailSpec extends FlatSpec with Matchers {
  behavior of "explict fail"

  it should "fail a test with an explicit failure code" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin

    val tester = TreadleTester(input)

    tester.fail(3)
    tester.report()
    tester.reportString should include ("Failed: Code 3")
  }
}
