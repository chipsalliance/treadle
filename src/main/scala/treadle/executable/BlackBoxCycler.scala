// See LICENSE for license details.

package treadle.executable

import treadle.ScalaBlackBox

case class BlackBoxCycler(
                           symbol: Symbol,
                           blackBox: ScalaBlackBox,
                           clockSymbol: Symbol,
                           dataStore: DataStore
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
