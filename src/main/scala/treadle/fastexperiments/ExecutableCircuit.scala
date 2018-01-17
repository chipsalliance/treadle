//// See LICENSE for license details.
//
//treadle treadle.fastexperiments
//
//import treadle.executable.{Assigner, Value, IntValue}
//
//import scala.collection.mutable
//
//class ExecutableCircuit {
//  val names: mutable.HashMap[String, Value] = new mutable.HashMap[String, Value]
//  val combinationalExpressions: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]
//  val registerExpressions: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]
//
//  def header: String = {
//    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
//  }
//
//  def addWire(wireValue: Value): Value = {
//    names(wireValue.name) = wireValue
//    wireValue
//  }
//
//  def apply(name: String): Value = {
//    names(name)
//  }
//
//  def getUInt(name: String): IntValue = {
//    names(name).asInstanceOf[IntValue]
//  }
//
//  override def toString: String = {
//    names.keys.toArray.sorted.map { key => f"${names(key).asBigInt}%10d" }.mkString("")
//  }
//}
