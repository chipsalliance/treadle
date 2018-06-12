// See LICENSE for license details.

package treadle.executable

import firrtl.ir.Info
import treadle.ScalaBlackBox

case class BlackBoxCycler(
  symbol      : Symbol,
  blackBox    : ScalaBlackBox,
  clockSymbol : Symbol,
  dataStore   : DataStore,
  info        : Info
)
extends Assigner {

  override def run: FuncUnit = {
    blackBox.cycle(PositiveEdge)
    if (isVerbose) {
      println(s"${symbol.name} : black box cycle($PositiveEdge)")
    }
    () => Unit
  }
}
