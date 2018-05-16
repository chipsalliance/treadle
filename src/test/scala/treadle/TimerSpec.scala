// See LICENSE for license details.

package treadle

import org.scalatest.{FlatSpec, Matchers}
import treadle.chronometry.Timer

// scalastyle:off magic.number
class TimerSpec extends FlatSpec with Matchers {
  behavior of "timer"

  it should "count times" in {
    val tag = "test1"
    val timer = new Timer
    timer.clear()
    timer(tag) {
      Thread.sleep(3000)
    }
    timer.timingLog.size should be (1)
    timer.timingLog(tag).events should be (1)
    timer.timingLog(tag).nanoseconds should be > 2000000000L
    println(timer.report())
  }
}
