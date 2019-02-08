// See LICENSE for license details.

package treadle.executable

import firrtl.WireKind
import firrtl.ir._

case class PrintfOp(
  symbol          : Symbol,
  info            : Info,
  string          : StringLit,
  args            : Seq[ExpressionResult],
  clockTransition : ClockTransitionGetter,
  condition       : IntExpressionResult
) extends Assigner {

  private val formatString = string.escape

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      val currentArgValues = args.map {
        case e: IntExpressionResult => e.apply()
        case e: LongExpressionResult => e.apply()
        case e: BigExpressionResult => e.apply()
      }
      val instantiatedString = executeVerilogPrint(formatString, currentArgValues)
      print(instantiatedString.drop(1).dropRight(1))
    }

    () => Unit
  }

  def executeVerilogPrint(formatString: String, allArgs: Seq[Any]): String = {
    val outBuffer = new StringBuilder
    var s = formatString
    var args = allArgs

    while(s.nonEmpty) {
      s.indexOf("%") match {
        case -1 =>
          outBuffer ++= s
          s = ""
        case offset =>
          outBuffer ++= s.take(offset)
          s = s.drop(offset + 1)
          s.headOption match {
            case Some('%') =>
              outBuffer ++= "%"
              s = s.tail
            case Some('b') =>
              outBuffer ++= BigInt(args.head.toString).toString(2)
              args = args.tail
              s = s.tail
            case Some('c') =>
              outBuffer += BigInt(args.head.toString).toChar
              args = args.tail
              s = s.tail
            case Some(specifier)   =>
              //noinspection ScalaUnnecessaryParentheses
              outBuffer ++= (s"%$specifier").format(BigInt(args.head.toString))
              args = args.tail
              s = s.tail
            case _ =>
              s = ""
          }
      }
    }
    StringContext.treatEscapes(outBuffer.toString())
  }
}

object PrintfOp {
  val PrintfOpSymbol = Symbol("printfop", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
}

case class PrintInfo(printSymbol: Symbol)