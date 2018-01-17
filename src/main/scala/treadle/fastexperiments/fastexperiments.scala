// See LICENSE for license details.

package treadle

package object fastexperiments {
  type Big = BigInt

  object Big {
    def apply(n: Int): Big = BigInt(n)
  }

  trait ExpressionResult

  type FuncInt = () => Int
  type FuncBig = () => Big

  trait Assigner {
    def apply(): Unit
  }
}
