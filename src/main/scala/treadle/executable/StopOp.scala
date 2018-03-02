// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info

case class StopOp(
    symbol          : Symbol,
    info            : Info,
    returnValue     : Int,
    condition       : ExpressionResult,
    clockExpression : IntExpressionResult,
    hasStopped      : Symbol,
    dataStore       : DataStore
) extends Assigner {

  //TODO: (chick) run should not use match, this should be determined statically

  var lastClockValue = clockExpression()

  def run: FuncUnit = {
    val clockValue = clockExpression()
    if(clockValue > 0 && lastClockValue == 0) {
      val conditionValue = condition match {
        case e: IntExpressionResult => e.apply() > 0
        case e: LongExpressionResult => e.apply() > 0L
        case e: BigExpressionResult => e.apply() > Big(0)
      }
      if (conditionValue) {
        if (isVerbose) {
          println(s"clock ${symbol.name} has fired")
        }
        dataStore(hasStopped) = returnValue + 1
      }
    }
    lastClockValue = clockValue

    () => Unit
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol, triggerSymbol: Symbol)