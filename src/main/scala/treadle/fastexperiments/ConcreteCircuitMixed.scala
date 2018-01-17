//// See LICENSE for license details.
//
//treadle treadle.fastexperiments
//
//import scala.collection.mutable
//
//case class Value(name: String, isSigned: Boolean, width: Int) {
//  var bigInt = BigInt(0)
//  var int    = 0
//  def applyInt(): Int = int
//  def applyBig(): BigInt = bigInt
//  def isBig: Boolean = width > 31
//}
//
//trait ValueFunction {
//  def applyInt: FuncInt = ???
//  def applyBig: FuncBig = ???
//}
//
//  trait Assigner
//
//class MixedCircuit {
//  private val values = new mutable.HashMap[String, Value]
//  private val functions = new mutable.ArrayBuffer[Assigner]()
//
//  def addAssign(name: String, function: ValueFunction): Unit = {
//    functions += AssignValues(name, function)
//  }
//
//  def getValue(name: String): Value = values(name)
//
//  def header: String = {
//    values.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
//  }
//  override def toString: String = {
//    values.keys.toArray.sorted.map(values(_).bigInt).map { b => f"$b%10d" }.mkString("")
//  }
//
//}
//
//case class GetValuesConstant(n: Int) extends ValueFunction {
//  def apply(): Int = n
//}
//
//case class GetValues(state: MixedCircuit, intValue: Value) extends ValueFunction {
//  val apply: ValueFunction = {
//    if(true) nakedGetValues else verboseGetValues
//  }
//
//  def nakedGetValues(): Int = {
//    intValue.value
//  }
//  def verboseGetValues(): Int = {
//    println(s"getting int from index ${intValue.value}")
//    intValue.value
//  }
//}
//
//case class AddValues(f1: ValueFunction, f2: ValueFunction) extends ValueFunction {
//  def applyInt(): Int = f1.applyInt f1() + f2()
//}
//
//case class SubValues(f1: ValueFunction, f2: ValueFunction) extends ValueFunction {
//  def apply(): Int = f1() - f2()
//}
//
//case class TailValues(f1: ValueFunction, f2: ValueFunction) extends ValueFunction {
//  def apply(): Int = f1()
//}
//
//case class MuxValues(condition: ValueFunction, trueClause: ValueFunction, falseClause: ValueFunction) extends ValueFunction {
//  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
//}
//
//case class EqValues(f1: ValueFunction, f2: ValueFunction) extends ValueFunction {
//  def apply(): Int = if(f1() == f2()) 1 else 0
//}
//
//case class GtValues(f1: ValueFunction, f2: ValueFunction) extends ValueFunction {
//  def apply(): Int = if(f1() > f2()) 1 else 0
//}
//
//case class AssignValues(name: String, expression: ValueFunction) extends Assigner {
//}
//
////noinspection ScalaStyle,ScalaUnusedSymbol
//object MixedCircuit {
//  def apply(nameMap: Map[String, UInt]): MixedCircuit = {
//    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
//      if(wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
//    }
//    new MixedCircuit
//  }
//
//  def runOnce(values: Seq[(Int, Int, Int)]): Unit ={
//    var nextWire = -1
//    def newNextWire() = { nextWire += 1; nextWire }
//
//    val wires = Seq(
//      Value("io_a", isSigned = false, 32),
//      Value("io_b", isSigned = false, 32),
//      Value("io_e", isSigned = false, 32),
//      Value("io_z", isSigned = false, 32),
//      Value("io_v", isSigned = false, 32),
//      Value("reg_x_in", isSigned = false, 32),
//      Value("reg_x_out", isSigned = false, 32),
//      Value("reg_y_in", isSigned = false, 32),
//      Value("reg_y_out", isSigned = false, 32),
//      Value("t_13", isSigned = false, 32),
//      Value("t_14", isSigned = false, 32),
//      Value("t_15", isSigned = false, 32),
//      Value("t_16", isSigned = false, 32),
//      Value("t_17", isSigned = false, 32),
//      Value("t_18", isSigned = false, 32),
//      Value("t_19", isSigned = false, 32),
//      Value("t_20", isSigned = false, 32),
//      Value("t_21", isSigned = false, 32),
//      Value("gen_0", isSigned = false, 32),
//      Value("gen_1", isSigned = false, 32)
//    )
//    val state = new MixedCircuit
//    wires.foreach { wire => state.names(wire.name) = wire}
//
////    println(s"state 0 $state")
//
//    state.addAssign("t_13",
//      GtValues(
//        GetValues("reg_x_out"),
//        GetValues("reg_x_in"),
//      )
//    )
//
//    val instructions = Seq(
//      AssignValues(state, state.getIndex("t_13"),
//        GtValues(
//          GetValues(state, state.getIndex("reg_x_out")).apply,
//          GetValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignValues(state, state.getIndex("t_14"),
//        SubValues(
//          GetValues(state, state.getIndex("reg_x_out")).apply,
//          GetValues(state, state.getIndex("reg_y_out")).apply).apply _
//      ),
//      AssignValues(state, state.getIndex("t_15"),
//        TailValues(
//          GetValues(state, state.getIndex("t_14")).apply,
//          GetValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("t_17"),
//        EqValues(
//          GetValues(state, state.getIndex("t_13")).apply,
//          GetValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("t_18"),
//        SubValues(
//          GetValues(state, state.getIndex("reg_y_out")).apply,
//          GetValues(state, state.getIndex("reg_x_out")).apply).apply _
//      ),
//      AssignValues(state, state.getIndex("t_19"),
//        TailValues(
//          GetValues(state, state.getIndex("t_18")).apply,
//          GetValuesConstant(1).apply _
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("t_21"),
//        EqValues(
//          GetValues(state, state.getIndex("reg_y_out")).apply,
//          GetValuesConstant(0).apply _
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("gen_0"),
//        MuxValues(
//          GetValues(state, state.getIndex("t_13")).apply,
//          GetValues(state, state.getIndex("t_15")).apply,
//          GetValues(state, state.getIndex("reg_x_out")).apply
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("gen_1"),
//        MuxValues(
//          GetValues(state, state.getIndex("t_17")).apply,
//          GetValues(state, state.getIndex("t_19")).apply,
//          GetValues(state, state.getIndex("reg_y_out")).apply
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("io_z"),
//        GetValues(state, state.getIndex("reg_x_out")).apply
//      ),
//      AssignValues(state, state.getIndex("io_v"),
//        GetValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignValues(state, state.getIndex("reg_x_in"),
//        GetValues(state, state.getIndex("t_21")).apply
//      ),
//      AssignValues(state, state.getIndex("reg_x_in"),
//        MuxValues(
//          GetValues(state, state.getIndex("io_e")).apply,
//          GetValues(state, state.getIndex("io_a")).apply,
//          GetValues(state, state.getIndex("gen_0")).apply
//        ).apply _
//      ),
//      AssignValues(state, state.getIndex("reg_y_in"),
//        MuxValues(
//          GetValues(state, state.getIndex("io_e")).apply,
//          GetValues(state, state.getIndex("io_b")).apply,
//          GetValues(state, state.getIndex("gen_1")).apply
//        ).apply _
//      )
//    )
//
//    val regNextInstructions = Seq(
//      AssignValues(state, state.getIndex("reg_x_out"),
//        GetValues(state, state.getIndex("reg_x_in")).apply
//      ),
//      AssignValues(state, state.getIndex("reg_y_out"),
//        GetValues(state, state.getIndex("reg_y_in")).apply
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
