// See LICENSE for license details.

package treadle.chronometry

import scala.collection.mutable

class UTC private (scaleName: String = "picoseconds") {
  private var internalTime: Long = 0L
  def currentTime:  Long = internalTime

  val eventQueue = new mutable.PriorityQueue[Event]()

  def addRecurringTask(period: Long, initialOffset: Long = 0)(thunk: () => Unit): Unit = {
    val task = RecurringTask(internalTime + initialOffset, period, thunk)
    eventQueue.enqueue(task)
  }

  def addOneTimeTask(time: Long)(thunk: () => Unit): Unit = {
    eventQueue.enqueue(OneTimeTask(time, thunk))
  }

  def hasNextTask: Boolean = {
    eventQueue.nonEmpty
  }

  def runNextTask(): Unit = {
    if(hasNextTask) {
      eventQueue.dequeue() match {
        case recurringTask: RecurringTask =>
          internalTime = recurringTask.time
          recurringTask.run()
          eventQueue.enqueue(recurringTask.copy(time = internalTime + recurringTask.period))
        case oneTimeTask: OneTimeTask =>
          internalTime = oneTimeTask.time
          oneTimeTask.run()
        case _ =>
          // do nothing
      }
    }
  }

  def advance(): Unit = {
    runNextTask()
  }

  def runUntil(time: Long): Unit = {
    while(eventQueue.nonEmpty && eventQueue.head.time <= time) {
      runNextTask()
    }
  }
}

object UTC {
  def apply(scaleName: String = "picoseconds"): UTC = new UTC(scaleName)
}

trait Event extends Ordered[Event] {
  def time: Long
  override def compare(that: Event): Int = {
    if(this.time < that.time) {
      1
    }
    else if(this.time == that.time) {
      0
    }
    else {
      -1
    }
  }
}

trait Task extends Event {
  def time: Long
  def run(): Unit
}

case class OneTimeTask(time: Long, thunk: () => Unit) extends Task {
  def run(): Unit = {
    thunk()
  }
}

case class RecurringTask(time: Long, period: Long, thunk: () => Unit) extends Task {
  def run(): Unit = {
    thunk()
  }
}
