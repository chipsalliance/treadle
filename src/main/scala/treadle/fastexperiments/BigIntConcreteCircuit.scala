//// See LICENSE for license details.
//
//treadle treadle.executable
//
//import scala.collection.mutable
//
//case class BigIntValue(name: String, isSigned: Boolean, size: Int) extends BaseValue {
//  var value: BigInt = 0
//  def apply(): BigInt = value
//}
//
//class BigIntConcreteCircuit {
//  val names: mutable.HashMap[String, BigIntValue] = new mutable.HashMap[String, BigIntValue]
//
//  def getIndex(name: String): BigIntValue = names(name)
//
//  def header: String = {
//    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
//  }
//  override def toString: String = {
//    names.keys.toArray.sorted.map(names(_).value).map { b => f"$b%10d" }.mkString("")
//  }
//}
//
//case class GetBigIntValuesConstant(n: BigInt) {
//  def apply(): BigInt = n
//}
//
//case class GetBigIntValues(state: BigIntConcreteCircuit, intValue: BigIntValue) {
//  val apply: () => BigInt = {
//    if(true) nakedGetBigIntValues else verboseGetBigIntValues
//  }
//
//  def nakedGetBigIntValues(): BigInt = {
//    intValue.value
//  }
//  def verboseGetBigIntValues(): BigInt = {
//    println(s"getting int from index ${intValue.value}")
//    intValue.value
//  }
//}
//
//case class AddBigIntValues(f1: () => BigInt, f2: () => BigInt) {
//  def apply(): BigInt = f1() + f2()
//}
//
//case class SubBigIntValues(f1: () => BigInt, f2: () => BigInt) {
//  def apply(): BigInt = f1() - f2()
//}
//
//case class TailBigIntValues(f1: () => BigInt, f2: () => BigInt) {
//  def apply(): BigInt = f1()
//}
//
//case class MuxBigIntValues(condition: () => BigInt, trueClause: () => BigInt, falseClause: () => BigInt) {
//  def apply(): BigInt = if(condition() > 0) trueClause() else falseClause()
//}
//
//case class EqBigIntValues(f1: () => BigInt, f2: () => BigInt) {
//  def apply(): BigInt = if(f1() == f2()) 1 else 0
//}
//
//case class GtBigIntValues(f1: () => BigInt, f2: () => BigInt) {
//  def apply(): BigInt = if(f1() > f2()) 1 else 0
//}
//
//case class AssignBigIntValues(state: BigIntConcreteCircuit, index: BigIntValue, expression: () => BigInt) extends Assigner {
//  def apply(): Unit = {
//    //    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
//    index.value = expression()
//  }
//}
//
////noinspection ScalaStyle,ScalaUnusedSymbol
//object BigIntConcreteCircuit {
//  def apply(nameMap: Map[String, IntValue]): BigIntConcreteCircuit = {
//    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
//      if(wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
//    }
//    new BigIntConcreteCircuit
//  }
//
//  def runOnce(values: Seq[(Int, Int, Int)]): Unit ={
//    var nextWire = -1
//    def newNextWire() = { nextWire += 1; nextWire }
//
//    val wires = Seq(
//      BigIntValue("io_a", isSigned = false, 32),
//      BigIntValue("io_b", isSigned = false, 32),
//      BigIntValue("io_e", isSigned = false, 32),
//      BigIntValue("io_z", isSigned = false, 32),
//      BigIntValue("io_v", isSigned = false, 32),
//      BigIntValue("reg_x_in", isSigned = false, 32),
//      BigIntValue("reg_x_out", isSigned = false, 32),
//      BigIntValue("reg_y_in", isSigned = false, 32),
//      BigIntValue("reg_y_out", isSigned = false, 32),
//      BigIntValue("t_13", isSigned = false, 32),
//      BigIntValue("t_14", isSigned = false, 32),
//      BigIntValue("t_15", isSigned = false, 32),
//      BigIntValue("t_16", isSigned = false, 32),
//      BigIntValue("t_17", isSigned = false, 32),
//      BigIntValue("t_18", isSigned = false, 32),
//      BigIntValue("t_19", isSigned = false, 32),
//      BigIntValue("t_20", isSigned = false, 32),
//      BigIntValue("t_21", isSigned = false, 32),
//      BigIntValue("gen_0", isSigned = false, 32),
//      BigIntValue("gen_1", isSigned = false, 32)
//    )
//    val state = new BigIntConcreteCircuit
//    wires.foreach { wire => state.names(wire.name) = wire}
//
////    println(s"state 0 $state")
//
//    val instructions = Seq(
//      AssignBigIntValues(state, state.getIndex("t_13"),
//        GtBigIntValues(
//          GetBigIntValues(state, state.getIndex("reg_x_out")).apply,
//          GetBigIntValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_14"),
//        SubBigIntValues(
//          GetBigIntValues(state, state.getIndex("reg_x_out")).apply,
//          GetBigIntValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_15"),
//        TailBigIntValues(
//          GetBigIntValues(state, state.getIndex("t_14")).apply,
//          GetBigIntValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_17"),
//        EqBigIntValues(
//          GetBigIntValues(state, state.getIndex("t_13")).apply,
//          GetBigIntValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_18"),
//        SubBigIntValues(
//          GetBigIntValues(state, state.getIndex("reg_y_out")).apply,
//          GetBigIntValues(state, state.getIndex("reg_x_out")).apply).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_19"),
//        TailBigIntValues(
//          GetBigIntValues(state, state.getIndex("t_18")).apply,
//          GetBigIntValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("t_21"),
//        EqBigIntValues(
//          GetBigIntValues(state, state.getIndex("reg_y_out")).apply,
//          GetBigIntValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("gen_0"),
//        MuxBigIntValues(
//          GetBigIntValues(state, state.getIndex("t_13")).apply,
//          GetBigIntValues(state, state.getIndex("t_15")).apply,
//          GetBigIntValues(state, state.getIndex("reg_x_out")).apply
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("gen_1"),
//        MuxBigIntValues(
//          GetBigIntValues(state, state.getIndex("t_17")).apply,
//          GetBigIntValues(state, state.getIndex("t_19")).apply,
//          GetBigIntValues(state, state.getIndex("reg_y_out")).apply
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("io_z"),
//        GetBigIntValues(state, state.getIndex("reg_x_out")).apply
//      ),
//      AssignBigIntValues(state, state.getIndex("io_v"),
//        GetBigIntValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignBigIntValues(state, state.getIndex("reg_x_in"),
//        GetBigIntValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignBigIntValues(state, state.getIndex("reg_x_in"),
//        MuxBigIntValues(
//          GetBigIntValues(state, state.getIndex("io_e")).apply,
//          GetBigIntValues(state, state.getIndex("io_a")).apply,
//          GetBigIntValues(state, state.getIndex("gen_0")).apply
//        ).apply _
//      ),
//      AssignBigIntValues(state, state.getIndex("reg_y_in"),
//        MuxBigIntValues(
//          GetBigIntValues(state, state.getIndex("io_e")).apply,
//          GetBigIntValues(state, state.getIndex("io_b")).apply,
//          GetBigIntValues(state, state.getIndex("gen_1")).apply
//        ).apply _
//      )
//    )
//
//    val regNextInstructions = Seq(
//      AssignBigIntValues(state, state.getIndex("reg_x_out"),
//        GetBigIntValues(state, state.getIndex("reg_x_in")).apply
//      ),
//      AssignBigIntValues(state, state.getIndex("reg_y_out"),
//        GetBigIntValues(state, state.getIndex("reg_y_in")).apply
//      )
//    )
//
//    def poke(name: String, value: Int): Unit = {
//      state.names(name).value = value
//    }
//    def peek(name: String): BigInt = {
//      state.names(name).value
//    }
//    def expect(name: String, value: Int, msg: => String) = {
//      assert(peek(name) == value,
//        s"${peek(name)} did not equal $value, $msg")
//    }
//
//    var cycle = 0
//    def step(): Unit = {
//      regNextInstructions.foreach { inst => inst() }
//      instructions.foreach { inst => inst() }
//      cycle += 1
//    }
//
//    def show(): Unit = {
//      println(f"state $cycle%6d $state")
//    }
//
////    println(f"state ${""}%6.6s  ${state.header}")
//
//    val startTime = System.nanoTime()
//
//    values.foreach { case (x, y, z) =>
//
//      poke("io_a", x)
//      poke("io_b", y)
//      poke("io_e", 1)
//
//      step()
//
//      poke("io_e", 0)
//      step()
//
//      while(peek("io_v") != 1) {
//        step()
//      }
//
//      expect("io_z", z, s"$x, $y")
//      //      show()
//
//    }
//
//    val endTime = System.nanoTime()
//    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0
//
//    println(
//      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
//    )
//  }
//
//  def main(args: Array[String]): Unit = {
//    def computeGcd(a: Int, b: Int): (Int, Int) = {
//      var x = a
//      var y = b
//      var depth = 1
//      while(y > 0 ) {
//        if (x > y) {
//          x -= y
//        }
//        else {
//          y -= x
//        }
//        depth += 1
//      }
//      (x, depth)
//    }
//
//    val values =
//      for {x <- 1 to 1000
//           y <- 1 to 1000
//      } yield (x, y, computeGcd(x, y)._1)
//
//    println("Running BigInt ConcreteCircuit")
//    runOnce(values)
//    runOnce(values)
//    runOnce(values)
//    println("Running Int ConcreteCircuit")
//    ConcreteCircuit.runOnce(values)
//    ConcreteCircuit.runOnce(values)
//    ConcreteCircuit.runOnce(values)
//  }
//}
