// See LICENSE for license details.

package treadle.executable

import scala.collection.mutable

class RollBackBuffer(dataStore: DataStore) extends HasDataArrays {
  var time: Long = 0L

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

class RollBackBufferRing(dataStore: DataStore) {
  val numberOfBuffers: Int = dataStore.numberOfBuffers
  val ringBuffer: Array[RollBackBuffer] = Array.fill(dataStore.numberOfBuffers)(new RollBackBuffer(dataStore))

  var oldestBufferIndex: Int = 0
  var latestBufferIndex: Int = 0

  def currentNumberOfBuffers: Int = (latestBufferIndex + numberOfBuffers - oldestBufferIndex) % numberOfBuffers

  def newestToOldestBuffers: Seq[RollBackBuffer] = {
    var list = List.empty[RollBackBuffer]
    if (latestBufferIndex != oldestBufferIndex) {
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

  def advanceAndGetNextBuffer(): RollBackBuffer = {
    latestBufferIndex += 1
    if(latestBufferIndex >= numberOfBuffers) {
      latestBufferIndex = 0
    }
    if(latestBufferIndex == oldestBufferIndex) {
      oldestBufferIndex += 1
      if(oldestBufferIndex >= numberOfBuffers) {
        oldestBufferIndex = 0
      }
    }
    ringBuffer(latestBufferIndex)
  }
}

/**
  * Manage a number of rollback buffers for each clock
  */
class RollBackBufferManager(dataStore: DataStore) {

  val clockToBuffers: mutable.HashMap[String, RollBackBufferRing] = new mutable.HashMap()

  def saveData(clockName: String, time: Long): Unit = {
    val buffer: RollBackBuffer = {
      if(! clockToBuffers.contains(clockName)) {
        clockToBuffers(clockName) = new RollBackBufferRing(dataStore)
      }
      clockToBuffers(clockName).advanceAndGetNextBuffer()
    }
    buffer.dump(time)
  }

  def findEarlierBuffer(clockName: String, time: Long): Option[RollBackBuffer] = {
    clockToBuffers.get(clockName) match {
      case Some(rollBackBufferRing) =>
        rollBackBufferRing.newestToOldestBuffers.find { buffer =>
          buffer.time < time
        }
      case _ =>
        None
    }
  }
}
