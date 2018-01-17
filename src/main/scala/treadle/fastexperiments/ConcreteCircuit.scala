// See LICENSE for license details.
//
//treadle treadle.fastexperiments
//
//import scala.collection.mutable
//
//trait BaseValue {
//  def name: String
//  def size: Int
//}
//
//case class IntValue(name: String, isSigned: Boolean, size: Int) extends BaseValue {
//  var value: Int = 0
//  def apply(): Int = value
//}
//
//case class BigValue(name: String, isSigned: Boolean, size: Int) extends BaseValue {
//  var value: BigInt = 0
//}
//
//class ConcreteCircuit {
//  val names: mutable.HashMap[String, IntValue] = new mutable.HashMap[String, IntValue]
//
//  def getIndex(name: String): IntValue = names(name)
//
//  def header: String = {
//    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
//  }
//  override def toString: String = {
//    names.keys.toArray.sorted.map(names(_).value).map { b => f"$b%10d" }.mkString("")
//  }
//}
//
//case class GetIntValuesConstant(n: Int) {
//  def apply(): Int = n
//}
//
//case class GetIntValues(state: ConcreteCircuit, intValue: IntValue) {
//  val apply: () => Int = {
//    if(true) nakedGetIntValues else verboseGetIntValues
//  }
//
//  def nakedGetIntValues(): Int = {
//    intValue.value
//  }
//  def verboseGetIntValues(): Int = {
//    println(s"getting int from index ${intValue.value}")
//    intValue.value
//  }
//}
//
//case class AddIntValues(f1: () => Int, f2: () => Int) {
//  def apply(): Int = f1() + f2()
//}
//
//case class SubIntValues(f1: () => Int, f2: () => Int) {
//  def apply(): Int = f1() - f2()
//}
//
//case class TailIntValues(f1: () => Int, f2: () => Int) {
//  def apply(): Int = f1()
//}
//
//case class MuxIntValues(condition: () => Int, trueClause: () => Int, falseClause: () => Int) {
//  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
//}
//
//case class EqIntValues(f1: () => Int, f2: () => Int) {
//  def apply(): Int = if(f1() == f2()) 1 else 0
//}
//
//case class GtIntValues(f1: () => Int, f2: () => Int) {
//  def apply(): Int = if(f1() > f2()) 1 else 0
//}
//
//case class AssignIntValues(state: ConcreteCircuit, index: IntValue, expression: () => Int) extends Assigner {
//  def apply(): Unit = {
//    //    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
//    index.value = expression()
//  }
//}
//
////noinspection ScalaStyle,ScalaUnusedSymbol
//object ConcreteCircuit {
//  def apply(nameMap: Map[String, UInt]): ConcreteCircuit = {
//    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
//      if(wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
//    }
//    new ConcreteCircuit
//  }
//
//  def runOnce(values: Seq[(Int, Int, Int)]): Unit ={
//    var nextWire = -1
//    def newNextWire() = { nextWire += 1; nextWire }
//
//    val wires = Seq(
//      IntValue("io_a", isSigned = false, 32),
//      IntValue("io_b", isSigned = false, 32),
//      IntValue("io_e", isSigned = false, 32),
//      IntValue("io_z", isSigned = false, 32),
//      IntValue("io_v", isSigned = false, 32),
//      IntValue("reg_x_in", isSigned = false, 32),
//      IntValue("reg_x_out", isSigned = false, 32),
//      IntValue("reg_y_in", isSigned = false, 32),
//      IntValue("reg_y_out", isSigned = false, 32),
//      IntValue("t_13", isSigned = false, 32),
//      IntValue("t_14", isSigned = false, 32),
//      IntValue("t_15", isSigned = false, 32),
//      IntValue("t_16", isSigned = false, 32),
//      IntValue("t_17", isSigned = false, 32),
//      IntValue("t_18", isSigned = false, 32),
//      IntValue("t_19", isSigned = false, 32),
//      IntValue("t_20", isSigned = false, 32),
//      IntValue("t_21", isSigned = false, 32),
//      IntValue("gen_0", isSigned = false, 32),
//      IntValue("gen_1", isSigned = false, 32)
//    )
//    val state = new ConcreteCircuit
//    wires.foreach { wire => state.names(wire.name) = wire}
//
////    println(s"state 0 $state")
//
//    val instructions = Seq(
//      AssignIntValues(state, state.getIndex("t_13"),
//        GtIntValues(
//          GetIntValues(state, state.getIndex("reg_x_out")).apply,
//          GetIntValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_14"),
//        SubIntValues(
//          GetIntValues(state, state.getIndex("reg_x_out")).apply,
//          GetIntValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_15"),
//        TailIntValues(
//          GetIntValues(state, state.getIndex("t_14")).apply,
//          GetIntValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_17"),
//        EqIntValues(
//          GetIntValues(state, state.getIndex("t_13")).apply,
//          GetIntValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_18"),
//        SubIntValues(
//          GetIntValues(state, state.getIndex("reg_y_out")).apply,
//          GetIntValues(state, state.getIndex("reg_x_out")).apply).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_19"),
//        TailIntValues(
//          GetIntValues(state, state.getIndex("t_18")).apply,
//          GetIntValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("t_21"),
//        EqIntValues(
//          GetIntValues(state, state.getIndex("reg_y_out")).apply,
//          GetIntValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("gen_0"),
//        MuxIntValues(
//          GetIntValues(state, state.getIndex("t_13")).apply,
//          GetIntValues(state, state.getIndex("t_15")).apply,
//          GetIntValues(state, state.getIndex("reg_x_out")).apply
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("gen_1"),
//        MuxIntValues(
//          GetIntValues(state, state.getIndex("t_17")).apply,
//          GetIntValues(state, state.getIndex("t_19")).apply,
//          GetIntValues(state, state.getIndex("reg_y_out")).apply
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("io_z"),
//        GetIntValues(state, state.getIndex("reg_x_out")).apply
//      ),
//      AssignIntValues(state, state.getIndex("io_v"),
//        GetIntValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignIntValues(state, state.getIndex("reg_x_in"),
//        GetIntValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignIntValues(state, state.getIndex("reg_x_in"),
//        MuxIntValues(
//          GetIntValues(state, state.getIndex("io_e")).apply,
//          GetIntValues(state, state.getIndex("io_a")).apply,
//          GetIntValues(state, state.getIndex("gen_0")).apply
//        ).apply _
//      ),
//      AssignIntValues(state, state.getIndex("reg_y_in"),
//        MuxIntValues(
//          GetIntValues(state, state.getIndex("io_e")).apply,
//          GetIntValues(state, state.getIndex("io_b")).apply,
//          GetIntValues(state, state.getIndex("gen_1")).apply
//        ).apply _
//      )
//    )
//
//    val regNextInstructions = Seq(
//      AssignIntValues(state, state.getIndex("reg_x_out"),
//        GetIntValues(state, state.getIndex("reg_x_in")).apply
//      ),
//      AssignIntValues(state, state.getIndex("reg_y_out"),
//        GetIntValues(state, state.getIndex("reg_y_in")).apply
//      )
//    )
//
//    def poke(name: String, value: Int): Unit = {
//      state.names(name).value = value
//    }
//    def peek(name: String): Int = {
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
//    runOnce(values)
//    runOnce(values)
//    runOnce(values)
////    ExecutableCircuit.runOnce(values)
////    ExecutableCircuit.runOnce(values)
////    ExecutableCircuit.runOnce(values)
//  }
//}
