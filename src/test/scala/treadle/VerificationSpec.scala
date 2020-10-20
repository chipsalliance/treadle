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

package treadle

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.executable.StopException
import treadle.stage.phases.IgnoreFormalAssumesAnnotation

class VerificationSpec extends AnyFreeSpec with Matchers {
  def input(doAssume: Boolean = false) = {
    val inject = if (doAssume) {
      "assume"
    } else {
      "assert"
    }
    s"""
       |circuit VerificationModule :
       |  module VerificationModule :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    output io : { flip in : UInt<8>, out : UInt<8>}
       |
       |    io.out <= io.in
       |    node _T = eq(io.in, UInt<2>("h3"))
       |    cover(clock, _T, UInt<1>("h1"), "cover 1\\n")
       |
       |    node _T_1 = eq(io.in, UInt<2>("h3"))
       |    $inject(clock, lt(io.in, UInt<8>("h7f")), UInt<1>("h1"), "input was not less that 0x7f")
       |
       |    when _T_1 :
       |      node _T_2 = neq(io.in, UInt<2>("h2"))
       |      assume(clock, _T_2, UInt<1>("h1"), "io.in is NOT 2\\n")
       |      node _T_3 = eq(io.out, io.in)
       |      assert(clock, _T_3, UInt<1>("h1"), "io.in is NOT io.out\\n")
       |
       |""".stripMargin
  }

  "verification formal statements should be handled as follows" - {
    "cover statements are removed" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input()), ShowFirrtlAtLoadAnnotation)) { _ => }
      }
      output.toString should not include ("cover")
    }

    "by default assume statements are converted to printf+stop during conversion to low firrtl" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input(doAssume = true)), ShowFirrtlAtLoadAnnotation)) { _ => }
      }
      output.toString should not include ("assume")
      output.toString should include(
        """    printf(clock, and(not(lt(io_in, UInt<8>("h7f"))), UInt<1>("h1")), "input was not less that 0x7f")
          |    stop(clock, and(not(lt(io_in, UInt<8>("h7f"))), UInt<1>("h1")), 66)
          |""".stripMargin
      )
    }

    "but IgnoreFormalAssumesAnnotation will drop assume statements" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(input(doAssume = true)), ShowFirrtlAtLoadAnnotation, IgnoreFormalAssumesAnnotation)
        ) { _ => }
      }
      output.toString should not include ("assume")
      output.toString should not include (
        """    printf(clock, and(not(lt(io_in, UInt<8>("h7f"))), UInt<1>("h1")), "input was not less that 0x7f")
          |    stop(clock, and(not(lt(io_in, UInt<8>("h7f"))), UInt<1>("h1")), 66)
          |""".stripMargin
        )
    }

    def runStopTest(firrtlString: String): Unit = {
      TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlString), ShowFirrtlAtLoadAnnotation)) { tester =>

        tester.poke("io_in", 77)
        tester.step()
        tester.expect("io_out", 77)

        tester.poke("io_in", 2)
        tester.step()
        tester.expect("io_out", 2)

        tester.poke("io_in", 3)
        tester.step()
        tester.expect("io_out", 3)

        tester.poke("io_in", 3)
        tester.step()
        tester.expect("io_out", 3)

        tester.poke("io_in", 0xf1)
        tester.step()
        tester.expect("io_out", 0xf1)

        tester.finish
      }
    }

    "failure of assert should generate stop 65" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        val result = intercept[StopException] {
          runStopTest(input())
        }
        result.message should include("Failure Stop: result 65")
      }
      output.toString should include("input was not less that 0x7f")
    }

    "failure of assume should generate stop 66" in {
      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        val result = intercept[StopException] {
          runStopTest(input(doAssume = true))
        }
        result.message should include("Failure Stop: result 66")
      }
      output.toString should include("input was not less that 0x7f")
    }
  }
}
