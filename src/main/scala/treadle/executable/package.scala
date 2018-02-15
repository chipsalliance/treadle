// See LICENSE for license details.

package treadle

package object executable {
  type Big = BigInt

  object Big {
    def apply(n: Int): Big = BigInt(n)
  }

  trait ExpressionResult

  type FuncInt  = () => Int
  type FuncLong = () => Long
  type FuncBig  = () => Big
  type FuncUnit = () => Unit

  trait Assigner {
    val symbol: Symbol
    var verboseAssign: Boolean = false
    def run: FuncUnit
    def setLeanMode(isLean: Boolean): Unit = {}
    def render: String = symbol.render
    def setVerbose(value: Boolean): Unit = {
      verboseAssign = value
    }
  }
}
