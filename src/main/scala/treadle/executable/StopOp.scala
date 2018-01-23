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
    dataStore     : DataStore
) extends Assigner {
  val triggerIndex = triggerSymbol.index

  //TODO: (chick) fun should not be matching, this should be determined statically

  def run: FuncUnit = {
    val conditionValue = condition match {
      case e: IntExpressionResult => e.apply() > 0
      case e: LongExpressionResult => e.apply() > 0L
      case e: BigExpressionResult => e.apply() > Big(0)
    }
    if(conditionValue && dataStore.currentIntArray(triggerIndex) > 0) {
      dataStore(StopOp.StopOpSymbol) = returnValue + 1
      throw StopException(s"Failed: Stop result $returnValue")
    }
    () => Unit
  }
}

object StopOp {
  val StopOpSymbol = Symbol("/stopped", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
  StopOpSymbol.cardinalNumber = Int.MaxValue
}

case class StopInfo(stopSymbol: Symbol, triggerSymbol: Symbol)