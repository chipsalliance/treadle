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

import java.io.File

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ShowLoweredFirrtlSpec extends AnyFreeSpec with Matchers {
  "provides a way to save off lowered firrtl that treadle will run on" in {
    val input =
      """
        |circuit DummyCircuit :
        |  module DummyCircuit :
        |    input reset : UInt<1>
        |    input in1 : UInt<2>
        |    output out1 : UInt<2>
        |    output out2 : UInt<2>
        |
        |    out1 <= in1
        |    node T_1 = add(out1, UInt<1>("h1"))
        |    out2 <= T_1
      """.stripMargin

    val directory = "test_run_dir/saved_low_firrtl"
    val firrtlFile = new File(s"$directory/DummyCircuit.treadle.lo.fir")
    if (firrtlFile.exists) {
      firrtlFile.delete()
    }
    TreadleTestHarness(
      Seq(FirrtlSourceAnnotation(input), SaveFirrtlAtLoadAnnotation, TargetDirAnnotation(directory))
    ) { _ => }

    firrtlFile.exists() should be(true)
  }
}
