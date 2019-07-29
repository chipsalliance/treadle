// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info
import treadle.ScalaBlackBox

/**
  * Implements an assigner that can be scheduled to publish clock transitions
  * to specific black box implementations
  * @param symbol symbol name of instance
  * @param blackBox the instance
  * @param clockSymbol clock used by instance
  * @param info source location
  */
//TODO: This should be called everytime something hanppens to clock and should indicate all possible transitions
case class BlackBoxCycler(
  symbol:                Symbol,
  blackBox:              ScalaBlackBox,
  clockSymbol:           Symbol,
  clockTransitionGetter: AbstractClockTransitionGetter,
  info:                  Info
)
extends Assigner {

  override def run: FuncUnit = {
    val transition = clockTransitionGetter.transition
    blackBox.clockChange(transition, clockSymbol.name)
    if (isVerbose) {
      println(s"${symbol.name} : clock ${clockSymbol.name} state ($transition)")
    }
    () => Unit
  }
}
