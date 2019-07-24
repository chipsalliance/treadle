// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info

case class StopOp(
  symbol             : Symbol,
  info               : Info,
  returnValue        : Int,
  condition          : IntExpressionResult,
  hasStopped         : Symbol,
  dataStore          : DataWriter,
  clockTransition    : ClockTransitionGetter
) extends Assigner {

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      if (isVerbose) {
        println(s"clock ${symbol.name} has fired")
      }
      dataStore.update(hasStopped, returnValue + 1)
    }

    () => Unit
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol)