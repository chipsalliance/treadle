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
  * @param dataStore the data
  * @param info source location
  */
//TODO: This should be called everytime something hanppens to clock and should indicate all possible transitions
case class BlackBoxCycler(
  symbol      : Symbol,
  blackBox    : ScalaBlackBox,
  clockSymbol : Symbol,
  dataStore   : DataStore,
  info        : Info
)
extends Assigner {

  override def run: FuncUnit = {
    blackBox.clockChange(PositiveEdge, clockSymbol.name)
    if (isVerbose) {
      println(s"${symbol.name} : black box cycle($PositiveEdge)")
    }
    () => Unit
  }
}
