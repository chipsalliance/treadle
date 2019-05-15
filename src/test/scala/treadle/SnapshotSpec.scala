// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class SnapshotSpec extends FreeSpec with Matchers {
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

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = false,
        vcdShowUnderscored = false,
        setVerbose = false,
        showFirrtlAtLoad = false,
        rollbackBuffers = 4,
        symbolsToWatch = Seq()
      )
    }

    val t = TreadleTester(input, optionsManager)
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

    snapshot1.contains(""""numberOfBuffers":4,""") should be (true)

    println(s"snapshot0\n$snapshot0")
    println(s"snapshot1\n$snapshot1")

    t.engine.dataStore.deserialize(snapshot0)

    val snapshot2 = t.engine.dataStore.serialize

    snapshot2 should be (snapshot0)

    println(s"snapshot2\n$snapshot2")
  }
}
