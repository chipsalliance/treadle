// See LICENSE for license details.

package treadle.executable

import scala.reflect.ClassTag

/**
  * [[DataBuffer]] implementation for the default [[DataStore]].
  * @param dataStore the dataStore to be backed up.
  */
class RollBackBuffer(dataStore: DataStore) extends HasDataArrays with DataBuffer {
  private var time: Long = 0L
  override def getTime: Long = time

  val intData    : Array[Int]  = Array.fill(dataStore.numberOfInts)(0)
  val longData   : Array[Long] = Array.fill(dataStore.numberOfLongs)(0)
  val bigData    : Array[Big]  = Array.fill(dataStore.numberOfBigs)(0)

  def dump(dumpTime: Long): Unit = {
    time = dumpTime
    Array.copy(dataStore.intData,  0, intData,  0, intData.length)
    Array.copy(dataStore.longData, 0, longData, 0, longData.length)
    Array.copy(dataStore.bigData,  0, bigData,  0, bigData.length)
  }
}

/**
  * Maintains a ring buffer of dataStore images
  * The only real complexity here is that the number of populated buffers is zero.
  * @param numberOfBuffers      size of ring
  * @param uninitializedBuffer  prototype for all uninitialized buffers in the ring
  */
class RollBackBufferRing[B <: DataBuffer](numberOfBuffers: Int, uninitializedBuffer: B)(implicit m: ClassTag[B]) {
  val ringBuffer: Array[B] = Array.fill[B](numberOfBuffers)(uninitializedBuffer)

  var oldestBufferIndex: Int = 0
  var latestBufferIndex: Int = 0

  def currentNumberOfBuffers: Int = {
    if(numberOfBuffers == 0) {
      0
    }
    else {
      (latestBufferIndex + numberOfBuffers - oldestBufferIndex) % numberOfBuffers
    }
  }

  /**
    * Return the buffers as a list in reverse time order.
    * @return
    */
  def newestToOldestBuffers: Seq[B] = {
    var list = List.empty[B]
    if(currentNumberOfBuffers > 0) {
      var index = latestBufferIndex
      while (index != oldestBufferIndex) {
        list = list :+ ringBuffer(index)
        index -= 1
        if (index < 0) {
          index = numberOfBuffers - 1
        }
      }
      list = list :+ ringBuffer(index)
    }
    list
  }

  /**
    * Advances the last buffer pointer and returns a buffer to be used for new data.
    * In the beginning this is an unused buffer, after the ring fills, it returns the oldest buffer
    * If the time parameter matches the most recent buffers time, that buffer will be re-used.
    * @param time the time that the returned buffer will be used to store d
    * @return
    */
  def advanceAndGetNextBuffer(time: Long): B = {
    if(currentNumberOfBuffers > 0 && time < ringBuffer(latestBufferIndex).getTime) {
      // It's an error to record something earlier in time
      throw TreadleException(s"rollback buffer requested has earlier time that last used buffer")
    }
    else if(currentNumberOfBuffers == 0 || (currentNumberOfBuffers > 0 && time > ringBuffer(latestBufferIndex).getTime)) {
      // time has advanced so get a new buffer or re-use the oldest one
      // if time did not advance just fall through and newest buffer to be used again
      latestBufferIndex += 1
      if (latestBufferIndex >= numberOfBuffers) {
        latestBufferIndex = 0
      }
      if (latestBufferIndex == oldestBufferIndex) {
        oldestBufferIndex += 1
        if (oldestBufferIndex >= numberOfBuffers) {
          oldestBufferIndex = 0
        }
      }
    }
    ringBuffer(latestBufferIndex)
  }
}

/**
  * Manage the allocation of the rollback buffers
  * @param numberOfBuffers size of ring
  * @param uninitializedBuffer  prototype for all uninitialized buffers in the ring
  */
class RollBackBufferManager[B <: DataBuffer](numberOfBuffers: Int, uninitializedBuffer: B)(implicit m: ClassTag[B]) {

  val rollBackBufferRing = new RollBackBufferRing(numberOfBuffers, uninitializedBuffer)

  /**
    * save current system state for a specific clock.
    * @param time the time that snapshot will be for
    */
  def saveData(time: Long): Unit = {
    val buffer = rollBackBufferRing.advanceAndGetNextBuffer(time)
    buffer.dump(time)
  }

  /**
    * Finds the most recent buffer for the specified clock that is older than the specified time
    * @param time      a time that the buffer must be older than
    * @return
    */
  def findEarlierBuffer(time: Long): Option[B] = {
    rollBackBufferRing.newestToOldestBuffers.find { buffer =>
      buffer.getTime < time
    }
  }

  /**
    * Finds a buffer where a previous buffer has a high clock and this one has a low clock,then return this.
    *
    * @param time        buffer time must be earlier (<) than time
    * @param clock       the value of the clock at time rollback buffer's time
    * @param prevClock   the previous value of the clock
    * @return
    */
  def findBufferBeforeClockTransition(time: Long, clock: Symbol, prevClock: Symbol): Option[B] = {
    var foundHighClock = false

    rollBackBufferRing.newestToOldestBuffers.find { buffer =>
      if(buffer.getTime >= time) {
        false
      }
      else if(foundHighClock && buffer(prevClock) == 0) {
        true
      }
      else if(buffer(clock) > 0) {
        foundHighClock = true
        false
      }
      else {
        foundHighClock = false
        false
      }
    }
  }

  /**
    * returns a Seq of rollback buffers
    * @return
    */
  def newestToOldestBuffers: Seq[B] = rollBackBufferRing.newestToOldestBuffers
}
