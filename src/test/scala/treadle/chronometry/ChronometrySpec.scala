// See LICENSE for license details.

package treadle.chronometry

import firrtl.WireKind
import firrtl.ir.{IntWidth, UIntType}
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable._

// scalastyle:off magic.number
class ChronometrySpec extends FreeSpec with Matchers {
  "UTC can schedule events for single clock" in {
    val utc = new UTC()
    val basicClock = ScheduledClock(Symbol("bob", UIntType(IntWidth(1)), WireKind ), 100)
    utc.register(basicClock)

    for(i <- 0 until 10) {
      val events = utc.nextClocks()
      events.length should be (1)
      println(events.head)
    }
  }

  "UTC can schedule events for two clocks, 1 to 3 ratio" in {
    val utc = new UTC()
    val basicClock = ScheduledClock(Symbol("bob", UIntType(IntWidth(1)), WireKind ), 100)
    val oneThirdClock = ScheduledClock(Symbol("carol", UIntType(IntWidth(1)), WireKind ), 300)
    utc.register(basicClock)
    utc.register(oneThirdClock)

    for(i <- 0 until 10) {
      val events = utc.nextClocks()
      if(i % 3 == 2) {
        events.length should be (2)

      }
      else {
        events.length should be (1)

      }
      println(s"Events at time ${utc.currentTime}")
      println(events.mkString("\n"))
    }
  }

  "How slow is one clock" in {
    val toDo = 10000000L

    val utc = new UTC()
    val clock0 = ScheduledClock(Symbol("dog", UIntType(IntWidth(1)), WireKind ), 100)
    utc.register(clock0)

    val startTime = System.currentTimeMillis()
    for(i <- 0L until toDo) {
      val events = utc.nextClocks()
      if(i % (toDo / 10) == 0) {
        println(s"Events at time ${utc.currentTime}")
        println(events.mkString("\n"))
      }
    }
    val stopTime = System.currentTimeMillis()

    val eps = toDo.toDouble / (stopTime - startTime)
    println(
      f"$toDo events in ${(stopTime - startTime) / 1000.0}%10.5f seconds," +
        f"rate = $eps%10.5f KHz utc = ${utc.currentTime}")
  }

  "How slow are three clocks" in {
    val toDo = 10000000L

    val utc = new UTC()
    val clock0 = ScheduledClock(Symbol("dog", UIntType(IntWidth(1)), WireKind ), 100)
    val clock1 = ScheduledClock(Symbol("cat", UIntType(IntWidth(1)), WireKind ), 300)
    val clock2 = ScheduledClock(Symbol("fox", UIntType(IntWidth(1)), WireKind ), 17)
    utc.register(clock0)
    utc.register(clock1)
    utc.register(clock2)

    val startTime = System.currentTimeMillis()
    for(i <- 0L until toDo) {
      val events = utc.nextClocks()
      if(i % (toDo / 10) == 0) {
        println(s"Events at time ${utc.currentTime}")
        println(events.mkString("\n"))
      }
    }
    val stopTime = System.currentTimeMillis()

    val eps = toDo.toDouble / (stopTime - startTime)
    println(f"$toDo events in ${(stopTime - startTime) / 1000.0}%10.5f seconds, rate = $eps%10.5f MHz utc = ${utc.currentTime}")
  }
}
