/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.chronometry

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.{PrefixPrintfWithWallTime, TreadleTestHarness, WriteVcdAnnotation}

class PrintfOnDerivedClockSpec extends AnyFreeSpec with Matchers with LazyLogging {
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
        |    printf(derived_clock, UInt<1>(1), "SPI spi.sck=%d derived_clock=%d DERIVED\n", spi.sck, out_from_clock)
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
        TargetDirAnnotation("test_run_dir/print_on_derived_clock"),
        OutputFileAnnotation("printf_on_derived_clock"),
        PrefixPrintfWithWallTime,
        WriteVcdAnnotation
      )

      TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
        tester.step() // currently without this the first printf does not happen :-(, reset is behind a register
        tester.poke("io_in", 1)
        tester.step()
        tester.poke("io_in", 0)
        tester.step()
        tester.poke("io_in", 1)
        tester.step()
        tester.poke("io_in", 0)
        tester.step(3)
      }
    }

    logger.debug(output.toString)

    // printf will be in output twice once for each up transition of io_in, which drives the derived clock
    output.toString.split("\n").count(line => line.contains("SPI spi.sck=")) should be(2)
  }

}
