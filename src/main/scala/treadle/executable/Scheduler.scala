// See LICENSE for license details.

package treadle.executable

import logger.LazyLogging
import treadle.TreadleException

import scala.collection.mutable

class Scheduler(val dataStore: DataStore, val symbolTable: SymbolTable) extends LazyLogging {
  var combinationalAssigns : mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  var clockedAssigns       : mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val orphanedAssigns      : mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  private val toAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()

  def addAssigner(symbol: Symbol, assigner: Assigner): Unit = {
    if(toAssigner.contains(symbol)) {
      throw new TreadleException(s"Assigner already exists for $symbol")
    }
    toAssigner(symbol) = assigner
  }

  def hasAssigner(symbol: Symbol): Boolean = {
    toAssigner.contains(symbol)
  }

  def allAssigners(): Seq[Assigner] = {
    toAssigner.values.toSeq
  }

  def inputChildrenAssigners(): Seq[Assigner] = {
    val assigners = {
      symbolTable.getChildren(symbolTable.inputPortsNames.map(symbolTable.nameToSymbol(_)).toSeq)
              .flatMap { symbol => toAssigner.get(symbol)}
              .toSeq
    }
    assigners
  }

  def getAssigners(symbols: Seq[Symbol]): Seq[Assigner] = {
    val assigners = symbols.flatMap { symbol => toAssigner.get(symbol) }
    assigners
  }

  def organizeAssigners(): Unit = {
    val orphansAndSensitives = symbolTable.orphans ++ symbolTable.getChildren(symbolTable.orphans)

    setOrphanedAssigners(getAssigners(orphansAndSensitives))
    combinationalAssigns ++= allAssigners()
    sortInputSensitiveAssigns()
  }

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setVerbose(isVerbose)
    }
    combinationalAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    combinationalAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
  }

  /**
    * Execute the seq of assigners
    * @param assigners list of assigners
    */
  def executeAssigners(assigners: Seq[Assigner]): Unit = {
    var index = 0
    val lastIndex = assigners.length
    // val t0 = System.nanoTime()
    while(index < lastIndex) {
      assigners(index).run()
      index += 1
    }
  }

  /**
    *  updates signals that depend on inputs
    */
  def executeActiveAssigns(): Unit = {
    executeAssigners(combinationalAssigns)
  }

  /**
    * de-duplicates and sorts assignments that depend on top level inputs.
    */
  def sortInputSensitiveAssigns(): Unit = {
    val deduplicatedAssigns = combinationalAssigns.distinct
    combinationalAssigns = deduplicatedAssigns.sortBy { assigner: Assigner =>
      assigner.symbol.cardinalNumber
    }
  }

  def setOrphanedAssigners(assigners: Seq[Assigner]): Unit = {
    orphanedAssigns.clear()
    orphanedAssigns ++= assigners
  }

  def addAssigner(assigner: Assigner, triggerOption: Option[Symbol] = None): Unit = {
    combinationalAssigns += assigner
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
    s"Active assigns (${combinationalAssigns.size})\n" +
    combinationalAssigns.map { assigner =>
      assigner.symbol.render
    }.mkString("\n") + "\n\n"
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
