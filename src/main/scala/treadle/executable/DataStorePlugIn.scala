// See LICENSE for license details.

package treadle.executable

abstract class DataStorePlugIn {
  def setEnabled(enabled: Boolean): Unit
  def isEnabled: Boolean
  def run(symbol: Symbol): Unit
}
