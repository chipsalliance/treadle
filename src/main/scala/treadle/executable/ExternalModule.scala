// See LICENSE for license details.

package treadle.executable

import firrtl.ir.{Param, Type}

import scala.collection._

/**
  * This is the template for writing scala functions that implement the behaviour of a
  * black box.  Implementing classes should add internal
  * variables to hold any state information.
  */
trait BlackBoxImplementation {
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
    * Called whenever the cycle command of the engine is called.
    */
  def cycle(transition: Transition): Unit = {}

  /**
    * returns a list of names of inputs that this output depends on.
    * @note The order of this list will determine the order of the inputValues argument to the execute method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  def outputDependencies(outputName: String): Seq[String]

  /**
    * Add any parameters to the black box implementation
    */
  def setParams(params: Seq[Param]): Unit = {
  }
}

/**
  * For each instantiation of an ExtModule the engine needs a separate instance of
  * a BlackBoxImplementation. This factory provides it.
  * @example {{{
  *   class ExampleBBFactory extends BlackBoxFactory {
  *     override def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
  *       instanceName match {
  *         case "bb1" => Some(add(new BB1Impl))
  *         case "bb2" => Some(add(new BB2Impl))
  *         case "bb3" => Some(add(new BB3Impl))
  *         case _ => throw Exception(s"ExampleBBBFactory does not know how to create " + instanceName)
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
}

case class BlackBoxCycler(
    symbol: Symbol,
    blackBox: BlackBoxImplementation,
    clockSymbol: Symbol,
    dataStore: DataStore
) extends Assigner {

  override def run: FuncUnit = {
    blackBox.cycle(PositiveEdge)
    if(isVerbose) {
      println(s"${symbol.name} : black box cycle($PositiveEdge)")
    }
    () => Unit
  }
}
