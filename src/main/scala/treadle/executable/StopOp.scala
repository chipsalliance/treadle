// See LICENSE for license details.

package treadle.executable

import firrtl.WireKind
import firrtl.ir.{Info, IntWidth, NoInfo, UIntType}
import treadle.{ExecutionEngine, StopException}

case class StopOp(
    symbol        : Symbol,
    info          : Info,
    returnValue   : Int,
    condition     : ExpressionResult,
    triggerSymbol : Symbol,
    hasStopped    : Symbol,
    dataStore     : DataStore
) extends Assigner {

  private val triggerIndex = triggerSymbol.index

  //TODO: (chick) fun should not be matching, this should be determined statically

  def run: FuncUnit = {
    val conditionValue = condition match {
      case e: IntExpressionResult => e.apply() > 0
      case e: LongExpressionResult => e.apply() > 0L
      case e: BigExpressionResult => e.apply() > Big(0)
    }
    if(conditionValue && dataStore.currentIntArray(triggerIndex) == 1) {
      dataStore(hasStopped) = returnValue + 1
      throw StopException(s"Failed: Stop result $returnValue")
    }
    () => Unit
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol, triggerSymbol: Symbol)