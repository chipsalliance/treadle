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

import firrtl._
import firrtl.annotations._
import firrtl.transforms._
import firrtl.stage._
import firrtl.passes.wiring._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BoreSpec extends AnyFreeSpec with Matchers {
  "WiringTransform should be honor" in {
    val input =
      """
        |circuit BoreTestTop :
        |  module BoreTestConstant :
        |    input clock : Clock
        |    input reset : Reset
        |
        |    wire x : UInt<6>
        |    x <= UInt<6>("h02a")
        |
        |  module BoreTestExpect :
        |    input clock : Clock
        |    input reset : Reset
        |    output y : UInt<6>
        |
        |    y <= UInt<1>("h00")
        |
        |  module BoreTestTop :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output y : UInt<6>
        |
        |    inst constant of BoreTestConstant @[BoreTest.scala 26:26]
        |    constant.clock <= clock
        |    constant.reset <= reset
        |    inst expect of BoreTestExpect @[BoreTest.scala 27:24]
        |    expect.clock <= clock
        |    expect.reset <= reset
        |    y <= expect.y @[BoreTest.scala 28:7]
        |""".stripMargin

    var annos: AnnotationSeq = Seq(
      FirrtlSourceAnnotation(input),
      SourceAnnotation(ComponentName("x", ModuleName("BoreTestConstant", CircuitName("BoreTestTop"))), "x"),
      DontTouchAnnotation(ReferenceTarget("BoreTestTop", "BoreTestConstant", List(), "x", List())),
      NoDedupAnnotation(ModuleName("BoreTestConstant", CircuitName("BoreTestTop"))),
      SinkAnnotation(ComponentName("y", ModuleName("BoreTestExpect", CircuitName("BoreTestTop"))), "x"),
      NoDedupAnnotation(ModuleName("BoreTestExpect", CircuitName("BoreTestTop"))),
      RunFirrtlTransformAnnotation(new firrtl.passes.wiring.WiringTransform)
    )

    annos = (new FirrtlStage).run(annos)
    val tester = TreadleTester(annos :+ TreadleFirrtlFormHint(LowForm))
    tester.expect("y", 42)
    tester.report()
  }
}
