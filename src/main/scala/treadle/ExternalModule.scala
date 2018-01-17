// See LICENSE for license details.

package treadle

import firrtl.ir.{Expression, Type, Width}
import treadle.executable.{Assigner, FuncUnit, Symbol}

import scala.collection._

/**
  * During dependency graph processing one of these will be created for each output of
  * each instantiated black box in the circuit
  * @param name The name of the output without module name prefix
  * @param implementation The implementation instance of the parent black box
  * @param dependentInputs The names of the inputs that this output depends on
  * @param tpe the concrete return type of this output
  */
case class BlackBoxOutput(name: String,
                          implementation: BlackBoxImplementation,
                          dependentInputs: Seq[String],
                          tpe: Type
                         ) extends Expression {
  def mapExpr(f: Expression => Expression): Expression = f(this)
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = this
  def execute(inputValues: Seq[BigInt]): BigInt = {
    implementation.execute(inputValues, tpe: Type, name)
  }
  def serialize: String = s"BlackBoxOutput($name,$tpe)"
}

/**
  * This is the template for writing scala functions that implement the behaviour of a
  * black box i.e. [[firrtl.ir.ExtModule]].  Implementing classes should add internal
  * variables to hold any state information.
  */
abstract class BlackBoxImplementation {
  def name: String
  def fullName(componentName: String): String = s"$name.$componentName"

  /**
    * Execute is called to determine the value for the named output at the
    * current state of the system.
    * @param inputValues This is a list of concrete values that are in the same order
    *                    as the outputDependencies lists them
    * @param tpe         The concrete type of this output
    * @param outputName  The name of this output
    * @return            Computed current concrete value for the name output
    */
  def execute(inputValues: Seq[BigInt], tpe: Type, outputName: String = ""): BigInt

  /**
    * Called whenever the cycle command of the interpreter is called.
    */
  def cycle(): Unit

  /**
    * returns a list of names of inputs that this output depends on.
    * @note The order of this list will determine the order of the inputValues argument to the execute method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  def outputDependencies(outputName: String): Seq[String]
}

/**
  * For each instantiation of an ExtModule the interpreter needs a separate instance of
  * a BlackBoxImplementation. This factory provides it.
  * @example {{{
  *   class ExampleBBFactory extends BlackBoxFactory {
  *     override def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
  *       instanceName match {
  *         case "bb1" => Some(add(new BB1Impl))
  *         case "bb2" => Some(add(new BB2Impl))
  *         case "bb3" => Some(add(new BB3Impl))
  *         case _ => throw Exception(s"ExampleBBBFactory does not know how to create $instanceName}")
  *       }
  *     }
  *   }
  * }}}
  */
abstract class BlackBoxFactory {
  val boxes: mutable.HashMap[String, BlackBoxImplementation] = new mutable.HashMap[String, BlackBoxImplementation]

  def add(blackBox: BlackBoxImplementation): BlackBoxImplementation = {
    boxes(blackBox.name) = blackBox
    blackBox
  }
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation]

  /**
    * the factory remembers the implementations it created so the interpreter uses this method to call the
    * cycle methods of each one
    */
  def cycle(): Unit = {
    boxes.values.foreach { box => box.cycle() }
  }
}

case class BlackBoxCycler(symbol: Symbol, blackBox: BlackBoxImplementation) extends Assigner {
  override def run: FuncUnit = {
    blackBox.cycle()
    () => Unit
  }
}
