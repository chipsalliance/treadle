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

package treadle.blackboxes

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.{BlackBoxFactoriesAnnotation, TreadleTester, WriteVcdAnnotation}

// scalastyle:off magic.number
class EicgWrapperSpec extends FreeSpec with Matchers {
  "Clock gate wrapper functions like and and with " in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesEicgWrapper :
        |  extmodule EICG_Wrapper :
        |    input in : Clock
        |    input en : UInt<1>
        |    output clk_out : UInt<1>
        |
        |    defname = EICG_Wrapper
        |
        |  module UsesEicgWrapper :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input enable : UInt<1>
        |    output count : UInt<17>
        |
        |    inst eicg_instance of EICG_Wrapper
        |
        |    node eicg_out = asClock(eicg_instance.clk_out)
        |
        |    reg counter : UInt<16>, eicg_out with : (reset => (reset, UInt(0)))
        |
        |    eicg_instance.in <= clock
        |    eicg_instance.en <= enable
        |
        |    counter <= add(counter, UInt(1))
        |
        |    count <= counter
        |
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory))
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    tester.poke("enable", 0)

    for (_ <- 0 to 20) {
      tester.step()
      tester.expect("count", 0)
    }

    tester.poke("enable", 1)

    for (trial <- 1 to 20) {
      tester.step()
      tester.expect("count", trial)
    }

    tester.poke("enable", 0)

    for (_ <- 1 to 20) {
      tester.step()
      tester.expect("count", 20)
    }

    tester.finish
  }
}
