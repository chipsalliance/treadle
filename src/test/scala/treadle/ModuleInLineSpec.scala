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

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleInLineSpec extends AnyFlatSpec with Matchers with LazyLogging {
  behavior.of("multiple modes")

  it should "expand instances as found" in {
    val stream = getClass.getResourceAsStream("/three_deep.fir")
    val input = io.Source.fromInputStream(stream).mkString

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.engine.symbolTable.outputPortsNames.size should be > 0
    }
  }

  it should "nester registers should all be using the same clock" in {
    val stream = getClass.getResourceAsStream("/NestedModsWithReg.fir")
    val input = io.Source.fromInputStream(stream).mkString

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      def testIt(): Unit = {
        Seq(1, 2, 3).foreach { n => tester.expect(s"out$n", 3) }
      }

      tester.poke("in1", 3)
      tester.step()
      testIt()
      tester.step()
      testIt()
      tester.step()
      testIt()
    }
  }
}
