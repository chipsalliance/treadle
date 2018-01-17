//// See LICENSE for license details.
//
//treadle treadle.executable
//
//import firrtl.WireKind
//import org.scalatest.{FreeSpec, Matchers}
//
//class DataStoreSpec extends FreeSpec with Matchers {
//  def makeSymbol(index: Int): Symbol = {
//    val s = new Symbol(s"n$index", IntSize, UnsignedInt, WireKind, 32, 1)
//    s.index = index
//    s
//  }
//  "DataStore holds all state information" - {
//    "can store data and retrieve for ints with 1 buffer" in {
//      val numInts = 10
//      val ds = new DataStore(numberOfBuffers = 1)
//      for(i <- 0 until numInts) ds.getIndex(IntSize)
//      ds.allocateBuffers()
//
//      val assigns = Seq.tabulate(numInts){ n => ds.AssignInt(makeSymbol(n), () => n).apply }
//      assigns.foreach { assign => assign() }
//
//      val gets  = Seq.tabulate(numInts){ n => ds.GetInt(n) }
//      gets.zipWithIndex.foreach { case (get, index) =>
//        get() should be (index)
//      }
//      for(index <- 0 until numInts) {
//        println(f"$index%4d" + ds.getIntRow(index).map { n => f"$n%5d" }.mkString(","))
//      }
//    }
//
//    "can store data and retrieve for ints with 4 buffer" in {
//      val numInts = 10
//      val buffers = 4
//      val ds = new DataStore(numberOfBuffers = buffers)
//      for(i <- 0 until numInts) ds.getIndex(IntSize)
//      ds.allocateBuffers()
//
//      for(pass <- 0 until 20) {
//        val assigns = Seq.tabulate(numInts) { n => ds.AssignInt(makeSymbol(n), () => n * 7 + pass) }
//        assigns.foreach { assign => assign.apply() }
//        ds.advanceBuffers()
//      }
//
//      for(index <- 0 until numInts) {
//        println(f"$index%4d" + ds.getIntRow(index).map { n => f"$n%5d" }.mkString(","))
//      }
//    }
//  }
//}
