// See LICENSE for license details.

package treadle.chronometry

import scala.collection.mutable
import treadle.executable._

class UTC(scaleName: String = "picoseconds") {
  private var time: Long = 0L
  def currentTime:  Long = time

  val eventQueue = new mutable.PriorityQueue[Event]()

  def register(clock: ScheduledClock): Unit = {
    eventQueue.enqueue(Event(clock.period + clock.offset, clock))
  }

  def nextEvent(nowEvents: List[Event] = List.empty): List[Event] = {
    if(nowEvents.isEmpty) {
      val event = eventQueue.dequeue()
      time = event.time
      nextEvent(event :: Nil)
    }
    else {
      eventQueue.headOption match {
        case Some(event) =>
          if(event.time == nowEvents.head.time) {
            nextEvent(eventQueue.dequeue() :: nowEvents)
          }
          else {
           nowEvents
          }
        case _ =>
          nowEvents
      }
    }
  }

  def nextClocks(): Seq[Symbol] = {
    val nowEvents = nextEvent()
    nowEvents.foreach { event => eventQueue.enqueue(Event(time + event.clock.period, event.clock))}
    nowEvents.map { event => event.clock.symbol }
  }
}

case class Event(time: Long, clock: ScheduledClock) extends Ordered[Event] {
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
