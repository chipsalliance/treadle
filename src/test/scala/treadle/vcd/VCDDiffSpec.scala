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

package treadle.vcd

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.options.ProgramArgsAnnotation
import treadle.vcd.diff.VcdDiffStage
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VCDDiffSpec extends AnyFreeSpec with Matchers {
  "VCDDiff should detect differences between two related vcd files" in {
    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      (new VcdDiffStage).run(
        Seq(
          ProgramArgsAnnotation("samples/test1.vcd"),
          ProgramArgsAnnotation("samples/test2.vcd")
        )
      )
    }
    val s = outputBuffer.toString
    s.contains("3                                 7   wire1                                     wire1") should be (true)
    s.contains("6                                 8   wire2                                     wire2") should be (true)
  }
}
