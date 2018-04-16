// See LICENSE for license details.

package treadle.executable

import treadle.ExecutionEngine
import treadle.vcd.VCD

abstract class DataStorePlugin {
  def executionEngine: ExecutionEngine
  def dataStore: DataStore
  var isEnabled: Boolean = false
  def setEnabled(enabled: Boolean): Unit = {
    isEnabled = enabled
    (isEnabled, dataStore.activePlugins.contains(this)) match {
      case (true, false) =>
        dataStore.activePlugins += this
      case (false, true) =>
        dataStore.activePlugins.remove(dataStore.activePlugins.indexOf(this))
      case _ =>
        /* do nothing */
    }
    if(dataStore.activePlugins.nonEmpty && dataStore.leanMode) {
      dataStore.leanMode = false
      executionEngine.scheduler.getAllAssigners.foreach { assigner => assigner.setLeanMode(false)}
    }
    else if(dataStore.activePlugins.isEmpty && ! dataStore.leanMode) {
      dataStore.leanMode = true
      executionEngine.scheduler.getAllAssigners.foreach { assigner => assigner.setLeanMode(true)}
    }
  }

  def run(symbol: Symbol, offset: Int = -1): Unit
}

class ReportAssignments(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  def run(symbol: Symbol, offset: Int = -1): Unit = {
    if(offset == -1) {
      val showValue = symbol.normalize(dataStore(symbol))
      println(s"${symbol.name} <= $showValue h${showValue.toString(16)}")
    }
    else {
      val showValue = symbol.normalize(dataStore(symbol, offset))
      println(s"${symbol.name}($offset) <= $showValue")
    }
  }
}

class RenderComputations(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  def run(symbol: Symbol, offset: Int = -1): Unit = {
    dataStore.executionEngineOption.foreach { executionEngine =>
      println(executionEngine.renderComputation(symbol.name))
    }
  }
}

class VcdHook(val executionEngine: ExecutionEngine, val vcd: VCD) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  override def run(symbol: Symbol, offset: Int = -1): Unit = {
    if(offset == -1) {
      val value = dataStore(symbol)
      vcd.wireChanged(symbol.name, value, width = symbol.bitWidth)
    }
  }
}
