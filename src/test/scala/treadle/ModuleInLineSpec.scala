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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleInLineSpec extends AnyFlatSpec with Matchers {
  behavior.of("multiple modes")

  it should "expand instances as found" in {
    val stream = getClass.getResourceAsStream("/three_deep.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.engine.symbolTable.outputPortsNames.size should be > 0
  }

  it should "nester registers should all be using the same clock" in {
    val stream = getClass.getResourceAsStream("/NestedModsWithReg.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.poke("in1", 3)
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")

    tester.report()
  }
}
