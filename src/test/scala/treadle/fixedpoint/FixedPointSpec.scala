// See LICENSE for license details.

package treadle.fixedpoint

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FlatSpec, Matchers}
import treadle._

class FixedPointSpec extends FlatSpec with Matchers {
  behavior of "dumb fixed point multiply test"

  it should "expand instances as found" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin


    val options = Seq()

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    tester.poke("a", BigInt("10", 2))
    tester.poke("b", BigInt("100", 2))
    tester.step()

    tester.expect("c", BigInt("1000", 2))
  }

  behavior of "allow zero length binary point"

  it should "be happy with zero" in {

    val input =
      """
        |circuit Unit :
        |  module Unit :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input io_in : Fixed<6><<0>>
        |    output io_out : Fixed
        |
        |    io_in is invalid
        |    io_out is invalid
        |    io_out <= io_in
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.poke("io_in", BigInt("11", 2))
    println(s"got ${tester.peek("io_out")}")
  }

  behavior of "set binary point"

  it should "shorten number with new binary point" in {
    val input =
      """
        |circuit SBP :
        |  module SBP :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : Fixed<6><<2>>, out : Fixed}
        |
        |    io is invalid
        |    node T_2 = bpset(io.in, 0)
        |    io.out <= T_2
      """.stripMargin

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.poke("io_in", BigInt("1011", 2))
    println(s"got ${tester.peek("io_out")}")
  }
}