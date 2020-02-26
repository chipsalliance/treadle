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

import treadle.utils.Render

import scala.collection.mutable

class UTC(val scaleName: String = "picoseconds") {
  private var internalTime: Long = 0L
  def currentTime:          Long = internalTime
  def setTime(time: Long): Unit = {
    if (internalTime < time || time == 0L) {
      internalTime = time
      if (isVerbose) Render.headerBar(s"WallTime: $internalTime")
      onTimeChange()
    }
  }

  var isVerbose: Boolean = false

  val eventQueue = new mutable.PriorityQueue[Task]()

  var onTimeChange: () => Unit = () => {}

  def addRecurringTask(period: Long, initialOffset: Long = 0, taskName: String = "")(thunk: () => Unit): Unit = {
    val task = RecurringTask(internalTime + initialOffset, period, taskName, thunk)
    eventQueue.enqueue(task)
  }

  def addOneTimeTask(time: Long, taskName: String = "")(thunk: () => Unit): Unit = {
    val task = OneTimeTask(time, taskName, thunk)
    eventQueue.enqueue(task)
  }

  def hasNextTask: Boolean = {
    eventQueue.nonEmpty
  }

  def runNextTask(): Option[Task] = {
    if (hasNextTask) {
      eventQueue.dequeue() match {
        case recurringTask: RecurringTask =>
          setTime(recurringTask.time)
          recurringTask.run()
          eventQueue.enqueue(recurringTask.copy(time = internalTime + recurringTask.period))
          Some(recurringTask)
        case oneTimeTask: OneTimeTask =>
          setTime(oneTimeTask.time)
          oneTimeTask.run()
          Some(oneTimeTask)
        case _ =>
          // do nothing
          None
      }
    } else {
      None
    }
  }

  def runToTask(taskName: String): Unit = {
    if (eventQueue.nonEmpty) {
      val done = eventQueue.head.taskName == taskName
      runNextTask()
      if (!done) runToTask(taskName)
    }
  }

  def runUntil(time: Long): Unit = {
    while (eventQueue.nonEmpty && eventQueue.head.time <= time) {
      runNextTask()
    }
    setTime(time)
  }

  def incrementTime(increment: Long): Unit = {
    runUntil(internalTime + increment)
  }
}

object UTC {
  def apply(scaleName: String = "picoseconds"): UTC = new UTC(scaleName)
}

trait Task extends Ordered[Task] {
  def taskName: String
  def run():    Unit
  def time:     Long
  override def compare(that: Task): Int = {
    if (this.time < that.time) {
      1
    } else if (this.time == that.time) {
      0
    } else {
      -1
    }
  }
}

case class OneTimeTask(time: Long, taskName: String, thunk: () => Unit) extends Task {
  def run(): Unit = {
    thunk()
  }
}

case class RecurringTask(time: Long, period: Long, taskName: String, thunk: () => Unit) extends Task {
  def run(): Unit = {
    thunk()
  }
}
