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

import treadle.chronometry.Timer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class TimerSpec extends AnyFlatSpec with Matchers {
  behavior.of("timer")

  it should "count times" in {
    val tag = "test1"
    val timer = new Timer
    timer.clear()
    timer(tag) {
      Thread.sleep(3000)
    }
    timer.timingLog.size should be(1)
    timer.timingLog(tag).events should be(1)
    timer.timingLog(tag).nanoseconds should be > 2000000000L
    println(timer.report())
  }
}
