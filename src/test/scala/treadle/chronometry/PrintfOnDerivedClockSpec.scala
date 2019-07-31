// See LICENSE for license details.

package treadle.chronometry

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.TreadleTester

class PrintfOnDerivedClockSpec extends FreeSpec with Matchers {
  "Printf in submodule in scope of withClock should appear in output" in {
    val input =
      """
        |circuit Outer :
        |  module Inner :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output spi : { flip sck : UInt<1>, out: UInt<1> }
        |
        |    spi.out <= spi.sck
        |    node derived_clock = asClock(spi.sck)
        |    node out_from_clock = asUInt(derived_clock)
        |    printf(derived_clock, UInt<1>(1), "SPI spi.sck=%d derived_clock=%d\n", spi.sck, out_from_clock)
        |
        |  module Outer :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : { flip in : UInt<1>, out: UInt<1> }
        |
        |    inst child of Inner
        |
        |    child.clock <= clock
        |    child.reset <= reset
        |    child.spi.sck <= io.in
        |    io.out <= child.spi.sck
        |
      """.stripMargin

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {
      val options = Seq(
        TopNameAnnotation("printf_on_derived_clock")
      )

      val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)
      tester.poke("io_in", 1)
      tester.step()
      tester.poke("io_in", 0)
      tester.step()
      tester.poke("io_in", 1)
      tester.step()
      tester.poke("io_in", 0)
      tester.step()
      tester.finish
    }

    // printf will be in output twice once for each up transition of io_in, which drives the derived clock
    output.toString.split("\n").count( line => line.contains("SPI spi.sck=")) should be (2)
  }

}
