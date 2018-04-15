// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info

case class StopOp(
  symbol             : Symbol,
  info               : Info,
  returnValue        : Int,
  condition          : IntExpressionResult,
  clockExpression    : IntExpressionResult,
  hasStopped         : Symbol,
  clockLastValue     : Symbol,
  dataStore          : DataStore
) extends Assigner {

  private val lastClockValueIndex = clockLastValue.index

  def run: FuncUnit = {
    val clockValue = clockExpression()
    val lastClockValue = dataStore.currentIntArray(lastClockValueIndex)

      val conditionValue = condition.apply() > 0
      if (conditionValue) {
        if (isVerbose) {
          println(s"clock ${symbol.name} has fired")
        }
        dataStore(hasStopped) = returnValue + 1
      }

    () => Unit
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol)