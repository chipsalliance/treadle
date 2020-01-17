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

import scala.collection.mutable

case class TimerEvent(tag: String) {
  var events = 0L
  var nanoseconds = 0L
  var lastEventNanoseconds = 0L
}

object Timer {
  val TenTo9th = 1000000000.0
}

class Timer {
  //noinspection VarCouldBeVal
  var enabled: Boolean = true
  val timingLog = new mutable.HashMap[String, TimerEvent]

  val totalEvent = TimerEvent("Total")

  def apply[R](tag: String)(block: => R): R = {
    if (enabled) {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()

      val timerEvent = timingLog.getOrElseUpdate(tag, TimerEvent(tag))
      timerEvent.events += 1
      totalEvent.events += 1
      val delta = t1 - t0
      timerEvent.nanoseconds += delta
      timerEvent.lastEventNanoseconds = delta
      totalEvent.nanoseconds += delta
      totalEvent.lastEventNanoseconds = delta
      result
    } else {
      block
    }
  }

  def entryFor(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) => s"${entry.events}:${entry.nanoseconds}:${entry.nanoseconds / entry.events}"
      case _           => ""
    }
  }

  def prettyEntry(entry: TimerEvent): String = {
    val total_seconds = entry.nanoseconds.toDouble / Timer.TenTo9th
    val averageSeconds = (entry.nanoseconds.toDouble / entry.events.toDouble) / Timer.TenTo9th * 1000000.0
    f"${entry.events}%10d events $total_seconds%12.6f total seconds $averageSeconds%12.6f average usec"
  }

  def prettyEntryForTag(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) =>
        prettyEntry(entry)
      case _ => ""
    }
  }

  def prettyLastTime(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) =>
        val lastEventSeconds = entry.lastEventNanoseconds.toDouble / Timer.TenTo9th
        s"$lastEventSeconds"
      case _ => ""
    }
  }

  def clear(): Unit = {
    timingLog.clear()
  }

  def report(): String = {
    val sortedTags = timingLog.keys.toSeq.sorted
    sortedTags.map { tag =>
      f"$tag%-20s ${prettyEntryForTag(tag)}"
    }.mkString("\n") + "\n" +
      f"${"Total"}%-20s ${prettyEntry(totalEvent)}"
  }
}
