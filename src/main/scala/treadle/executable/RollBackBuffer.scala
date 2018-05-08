// See LICENSE for license details.

package treadle.executable

import scala.collection.mutable

class RollBackBuffer(intSize: Int, longSize: Int, bigIntSize: Int) {
  var time: Long = 0L
  var associatedClock: String = ""

  val intData    : Array[Int]  = Array.fill(intSize)(0)
  val longData   : Array[Long] = Array.fill(longSize)(0)
  val bigData    : Array[Big]  = Array.fill(bigIntSize)(0)

  def dump(dumpTime: Long, ints: Array[Int], longs: Array[Long], bigs: Array[Big]): Unit = {
    assert(intData.length == ints.length,
      s"RollBackBuffer.dump size error source ${ints.length} target ${intData.length}")
    assert(longData.length == longs.length,
      s"RollBackBuffer.dump size error source ${longs.length} target ${longData.length}")
    assert(bigData.length == bigs.length,
      s"RollBackBuffer.dump size error source ${bigs.length} target ${bigData.length}")

    Array.copy(ints,  0, intData,  0, intData.length)
    Array.copy(longs, 0, longData, 0, longData.length)
    Array.copy(bigs,  0, bigData,  0, bigData.length)
  }
}

class RollBackBufferManager(numberOfRollBackBuffers: Int) {
  val clockToBuffers: mutable.HashMap[String, Seq[RollBackBuffer]] = new mutable.HashMap()

  def dump(clockName: String, time: Long, dataStore: DataStore): Unit = {

  }

  def backupDepth: Int = ???


}
