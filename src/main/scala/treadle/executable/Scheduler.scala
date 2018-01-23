// See LICENSE for license details.

package treadle.executable

import logger.LazyLogging

import scala.collection.mutable

class Scheduler(val dataStore: DataStore, val symbolTable: SymbolTable) extends LazyLogging {
  var inputDependentAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val orphanedAssigns:       mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.verboseAssign = isVerbose
    }
    inputDependentAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    inputDependentAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
  }

  /**
    * Execute the seq of assigners
    * @param assigners list of assigners
    */
  def executeAssigners(assigners: Seq[Assigner]): Unit = {
    var index = 0
    val lastIndex = assigners.length
    while(index < lastIndex) {
      assigners(index).run()
      index += 1
    }
  }

  /**
    *  updates signals that depend on inputs
    */
  def executeInputSensitivities(): Unit = {
    executeAssigners(inputDependentAssigns)
  }

  /**
    * de-duplicates and sorts assignments that depend on top level inputs.
    */
  def sortInputSensitiveAssigns(): Unit = {
    val deduplicatedAssigns = inputDependentAssigns.distinct
    inputDependentAssigns = deduplicatedAssigns.sortBy { assigner: Assigner =>
      assigner.symbol.cardinalNumber
    }
  }

  def setOrphanedAssigners(assigners: Seq[Assigner]): Unit = {
    orphanedAssigns.clear()
    orphanedAssigns ++= assigners
  }

  /**
    * Render the assigners managed by this scheduler
    * @return
    */
  def render: String = {
    s"Static assigns (${orphanedAssigns.size})\n" +
      orphanedAssigns.map { assigner =>
        assigner.symbol.render
      }.mkString("\n") + "\n\n" +
    s"Combinational assigns (${inputDependentAssigns.size})\n" +
    inputDependentAssigns.map { assigner =>
      assigner.symbol.render
    }.mkString("\n") + "\n\n"
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
