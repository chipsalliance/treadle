// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info

case class StopOp(
  symbol             : Symbol,
  info               : Info,
  returnValue        : Int,
  condition          : IntExpressionResult,
  hasStopped         : Symbol,
  dataStore          : DataStore,
  clockTransition    : ClockTransitionGetter
) extends Assigner {

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      if(dataStore(hasStopped) > 0) {
        if (isVerbose) {
          println(s"previous stop has fired with result ${dataStore(hasStopped)}")
        }
      }
      else {
        if (isVerbose) {
          println(s"stop ${symbol.name} has fired")
        }
        dataStore(hasStopped) = returnValue + 1
      }
    }

    () => Unit
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol)