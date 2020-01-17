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

package treadle.chronometry

import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class ChronometrySpec extends FreeSpec with Matchers {
  "UTC can schedule a single task" in {
    val utc = UTC()
    var x = 0

    utc.hasNextTask should be(false)

    utc.addOneTimeTask(10) { () =>
      println(s"task number one")
      x = 1
    }

    utc.hasNextTask should be(true)

    utc.runNextTask()

    x should be(1)

    utc.hasNextTask should be(false)
  }

  "A clock requires two recurring tasks, one for rising one for falling" in {
    val utc = UTC()
    var cycle = 0

    utc.addRecurringTask(1000, 500) { () =>
      println(s"clock up at ${utc.currentTime}")
      cycle += 1
    }

    println(s"first tasks at " + utc.eventQueue.head.time)

    utc.addRecurringTask(1000, 1000) { () =>
      println(s"clock down at ${utc.currentTime}")
    }

    for (i <- 1 to 10) {
      utc.runNextTask()
      utc.currentTime should be((i - 1) * 1000 + 500)
      cycle should be(i)
      utc.runNextTask()

      utc.hasNextTask should be(true)
    }
  }

  "API supports run until task name" in {
    val utc = UTC()
    var cycle = 0

    utc.addRecurringTask(1000, 500, "clock/up") { () =>
      println(s"clock up at ${utc.currentTime}")
      cycle += 1
    }

    utc.addRecurringTask(1000, 1000) { () =>
      println(s"clock down at ${utc.currentTime}")
      cycle += 7
    }

    utc.addOneTimeTask(155, "samuel") { () =>
      println(s"one time task samuel")
      cycle += 4
    }

    utc.runToTask("clock/up")

    cycle should be(5)

  }

  "UTC can schedule events for two clocks, 1 to 3 ratio" in {
    val utc = UTC()
    var cycleA = 0
    var cycleB = 0

    utc.addRecurringTask(1000) { () =>
      println(s"clock fast up   at ${utc.currentTime}")
      cycleA += 1
    }

    utc.addRecurringTask(1000, 500) { () =>
      println(s"clock fast down at ${utc.currentTime}")
    }

    utc.addRecurringTask(3000) { () =>
      println(s"clock slow up   at ${utc.currentTime}")
      cycleB += 1
    }

    utc.addRecurringTask(3000, 1500) { () =>
      println(s"clock slow down at ${utc.currentTime}")
    }

    for (_ <- 0 to 30) {
      utc.runNextTask()
    }

    cycleA should be(cycleB * 3)
  }

  "How slow is one clock" in {
    val toDo = 10000000L

    val utc = UTC()
    var cycle = 0

    utc.addRecurringTask(1000, 500) { () =>
      cycle += 1
    }

    utc.addRecurringTask(1000, 1000) { () =>
      }

    val startTime = System.currentTimeMillis()
    for (_ <- 1L to toDo) {
      utc.runNextTask()
    }
    val stopTime = System.currentTimeMillis()

    val eps = toDo.toDouble / (stopTime - startTime)
    println(
      f"$toDo events in ${(stopTime - startTime) / 1000.0}%10.5f seconds," +
        f"rate = $eps%10.5f KHz utc = ${utc.currentTime}"
    )

    //TODO: Figure out a way to do this that doesn't break CI
    // eps should be > 1000.0  // clock should be fairly lean, i.e. be at least a mega-hz
  }

  "How slow are three clocks" in {
    val toDo = 10000000L

    val utc = UTC()
    var cycle = 0

    utc.addRecurringTask(1000, 500) { () =>
      cycle += 1
    }

    utc.addRecurringTask(1000, 1000) { () =>
      }

    utc.addRecurringTask(3000, 2000) { () =>
      cycle += 1
    }

    utc.addRecurringTask(3000, 1000) { () =>
      }

    utc.addRecurringTask(900, 500) { () =>
      cycle += 1
    }

    utc.addRecurringTask(900, 1000) { () =>
      }

    val startTime = System.currentTimeMillis()
    for (_ <- 1L to toDo) {
      utc.runNextTask()
    }
    val stopTime = System.currentTimeMillis()

    val eps = toDo.toDouble / (stopTime - startTime)
    println(
      f"$toDo events in ${(stopTime - startTime) / 1000.0}%10.5f seconds," +
        f"rate = $eps%10.5f KHz utc = ${utc.currentTime}"
    )

    //TODO: Figure out a way to do this that doesn't break CI
    // eps should be > 1000.0  // clock should be fairly lean, i.e. be at least a mega-hz
  }

}
