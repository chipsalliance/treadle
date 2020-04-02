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
import treadle.{BlackBoxFactoriesAnnotation, PlusArgsAnnotation, TreadleTester, WriteVcdAnnotation}

// scalastyle:off magic.number
class PlusArgReaderSpec extends FreeSpec with Matchers {
  "plus-args allow changes to values inside of compiled modules" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesPlusArgReader :
        |  extmodule PlusArgReader :
        |    output out : UInt<128>
        |
        |    defname = PlusArgReader
        |
        |    parameter DEFAULT = 1
        |    parameter WIDTH = 32
        |    parameter FORMAT = "value=%d"
        |
        |  module UsesPlusArgReader :
        |    input clock : Clock
        |    output out : UInt<128>
        |
        |    inst plusArgReader of PlusArgReader
        |
        |    out <= plusArgReader.out
        |
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory)),
      PlusArgsAnnotation(Seq("+value=11"))
    )

    val tester = TreadleTester(FirrtlSourceAnnotation(input) +: options)

    tester.expect("out", BigInt(11))

    tester.finish
  }
}
