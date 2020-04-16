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
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class SnapshotSpec extends AnyFreeSpec with Matchers {
  "Snapshots can be created" in {
    val input =
      """
        |circuit SnapShotExample :
        |  module SnapShotExample :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    output out0 : UInt<16>
        |    output out1 : UInt<44>
        |    output out2 : UInt<128>
        |    output out3 : UInt<128>
        |
        |    reg r1 : UInt<16>, clock
        |    reg r2 : UInt<44>, clock
        |    reg r3 : UInt<128>, clock
        |
        |    r1 <= in0
        |    r2 <= r1
        |    r3 <= r2
        |
        |    out0 <= in0
        |    out1 <= r1
        |    out2 <= r2
        |    out3 <= r3
        |
      """.stripMargin

    val t = TreadleTester(Seq(FirrtlSourceAnnotation(input), RollBackBuffersAnnotation(4)))

    t.poke("in0", 1)
    t.step()
    t.poke("in0", 2)
    t.step()
    t.poke("in0", 3)

    val snapshot0 = t.engine.dataStore.serialize

    t.step()
    t.poke("in0", 4)
    t.step()
    t.poke("in0", 5)
    t.step()

    val snapshot1 = t.engine.dataStore.serialize

    println(s"snapshot0\n$snapshot0")
    println(s"snapshot1\n$snapshot1")

    snapshot1.contains(""""numberOfBuffers":4,""") should be(true)

    println(s"snapshot0\n$snapshot0")
    println(s"snapshot1\n$snapshot1")

    t.engine.dataStore.deserialize(snapshot0)

    val snapshot2 = t.engine.dataStore.serialize

    snapshot2 should be(snapshot0)

    println(s"snapshot2\n$snapshot2")
  }
}
