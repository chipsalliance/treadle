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

import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.annotations._
import firrtl.stage.FirrtlSourceAnnotation
import logger.{LazyLogging, LogLevel, Logger}
import org.scalatest.freespec.AnyFreeSpec

class MemoryInitializationTest extends AnyFreeSpec with LazyLogging {
  "Memories should be loadable using annotations" in {
    val firrtlText =
      """
        |;buildInfoPackage: chisel3, version: 3.4-SNAPSHOT, scalaVersion: 2.12.11, sbtVersion: 1.3.10
        |circuit HasMemories :
        |  module HasMemories :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 28:20]
        |    cmem memory2 : UInt<4>[16] @[MemoryLoaderTest.scala 29:20]
        |    cmem memory3 : UInt<4>[16] @[MemoryLoaderTest.scala 30:20]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 36:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 36:8]
        |    infer mport _T_1 = memory2[address], clock @[MemoryLoaderTest.scala 37:18]
        |    out2 <= _T_1 @[MemoryLoaderTest.scala 37:8]
        |    infer mport _T_2 = memory3[address], clock @[MemoryLoaderTest.scala 38:18]
        |    out3 <= _T_2 @[MemoryLoaderTest.scala 38:8]
        |
        |    """.stripMargin

    def getMemoryReference(memoryNumber: Int): ReferenceTarget = {
      CircuitTarget("HasMemories").module("HasMemories").ref(s"memory$memoryNumber")
    }

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(firrtlText),
        MemoryRandomInitAnnotation(getMemoryReference(1)),
        MemoryScalarInitAnnotation(getMemoryReference(2), 7),
        MemoryArrayInitAnnotation(getMemoryReference(3), Seq.tabulate(16) { i =>
          i
        })
      )
    )

    Logger.setLevel(classOf[MemoryInitializationTest], LogLevel.Info)
    for (i <- 0 until 16) {
      tester.poke("address", i)
      tester.step()
      logger.info(s"${tester.peek("out1")} : ${tester.peek("out2")} : ${tester.peek("out3")}")
    }
  }

  "Memories should be loadable even in submodules" in {
    val firrtlText =
      """
        |;buildInfoPackage: chisel3, version: 3.4-SNAPSHOT, scalaVersion: 2.12.11, sbtVersion: 1.3.10
        |circuit Topper :
        |  module Bottomer :
        |    input clock : Clock
        |    input reset : Reset
        |    input address : UInt<4>
        |    output out : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 46:20]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 49:17]
        |    out <= _T @[MemoryLoaderTest.scala 49:7]
        |
        |  module Middler :
        |    input clock : Clock
        |    input reset : Reset
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 54:20]
        |
        |    inst bottomModule of Bottomer @[MemoryLoaderTest.scala 56:28]
        |    bottomModule.clock <= clock
        |    bottomModule.reset <= reset
        |    bottomModule.address <= address @[MemoryLoaderTest.scala 57:24]
        |
        |    inst bottomModule2 of Bottomer @[MemoryLoaderTest.scala 56:28]
        |    bottomModule2.clock <= clock
        |    bottomModule2.reset <= reset
        |    bottomModule2.address <= address @[MemoryLoaderTest.scala 57:24]
        |
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 61:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 61:8]
        |    out2 <= bottomModule.out @[MemoryLoaderTest.scala 62:8]
        |    out3 <= bottomModule2.out @[MemoryLoaderTest.scala 62:8]
        |
        |  module Topper :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |    output out4 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 68:20]
        |    inst middleModule of Middler @[MemoryLoaderTest.scala 70:28]
        |    middleModule.clock <= clock
        |    middleModule.reset <= reset
        |    middleModule.address <= address @[MemoryLoaderTest.scala 71:24]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 77:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 77:8]
        |    out2 <= middleModule.out1 @[MemoryLoaderTest.scala 78:8]
        |    out3 <= middleModule.out2 @[MemoryLoaderTest.scala 79:8]
        |    out4 <= middleModule.out3 @[MemoryLoaderTest.scala 79:8]
        |
        |""".stripMargin

    val specificReference = ReferenceTarget(
      "Topper",
      "Bottomer",
      Seq(
        (Instance("middleModule"), OfModule("Middler")),
        (Instance("bottomModule2"), OfModule("Bottomer"))
      ),
      "memory1",
      Seq.empty
    )

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(firrtlText),
        SaveFirrtlAtLoadAnnotation,
        MemoryScalarInitAnnotation(
          CircuitTarget("Topper").module("Topper").ref("memory1"),
          7
        ),
        MemoryArrayInitAnnotation(
          CircuitTarget("Topper").module("Middler").ref("memory1"),
          Seq.tabulate(16) { i =>
            i
          }
        ),
        MemoryArrayInitAnnotation(
          specificReference,
          Seq.tabulate(16) { i =>
            15 - i
          }
        )
      )
    )

    Logger.setLevel(classOf[MemoryInitializationTest], LogLevel.Info)
    for (i <- 0 until 16) {
      tester.poke("address", i)
      tester.step()

      logger.info(
        s"${tester.peek("out1")} : ${tester.peek("out2")} :" +
          s" ${tester.peek("out3")} : ${tester.peek("out4")}"
      )

      for (index <- 0 until 16) {
        tester.expect("out1", 7)
        tester.expect("out2", i)
        tester.expect("out3", 0)
        tester.expect("out4", 15 - i)
      }
    }
  }
}
