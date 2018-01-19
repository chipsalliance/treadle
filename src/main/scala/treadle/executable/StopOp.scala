// See LICENSE for license details.

package treadle.executable

import firrtl.WireKind
import firrtl.ir.{Info, IntWidth, NoInfo, UIntType}
import treadle.ExecutionEngine

case class StopOp(
    info       : Info,
    returnValue: Int,
    condition  : ExpressionResult,
    dataStore  : DataStore
) extends Assigner {

  val symbol: Symbol = StopOp.StopOpSymbol

  def run: FuncUnit = {
    val conditionValue = condition match {
      case e: IntExpressionResult => e.apply() > 0
      case e: LongExpressionResult => e.apply() > 0L
      case e: BigExpressionResult => e.apply() > Big(0)
    }
    if (conditionValue) {
      dataStore(symbol) = returnValue + 1
    }
    () => Unit
  }
}

object StopOp {
  val StopOpSymbol = Symbol("/stopped", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
  StopOpSymbol.cardinalNumber = Int.MaxValue
}