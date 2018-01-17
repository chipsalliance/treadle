// See LICENSE for license details.

package treadle.executable

import logger.LazyLogging

import scala.collection.mutable

class Scheduler(val dataStore: DataStore, val symbolTable: SymbolTable) extends LazyLogging {
  var inputDependentAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val orphanedAssigns:       mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  /**
    * associates a Symbol with a bunch of assignments
    * that happen on leading edge of that expression
    */
  val triggeredAssigns: mutable.HashMap[Symbol, mutable.ArrayBuffer[Assigner]] = {
    new mutable.HashMap[Symbol, mutable.ArrayBuffer[Assigner]] {
      override def default(key: Symbol): mutable.ArrayBuffer[Assigner] = {
        this(key) = new mutable.ArrayBuffer[Assigner]()
        this(key)
      }
    }
  }

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.verboseAssign = isVerbose
    }
    inputDependentAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
    triggeredAssigns.values.foreach { assigners =>
      assigners.foreach { setMode }
    }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    inputDependentAssigns.foreach { setMode }
    orphanedAssigns.foreach { setMode }
    triggeredAssigns.values.foreach { assigners =>
      assigners.foreach { setMode }
    }
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

  def executeTriggeredAssigns(symbol: Symbol): Unit = {
    val triggerValue = symbol.dataSize match {
      case IntSize  => dataStore.currentIntArray(symbol.index) > 0
      case LongSize => dataStore.currentLongArray(symbol.index) > 0L
      case BigSize  => dataStore.currentBigArray(symbol.index) > Big(0)
    }
    if(triggerValue) {
      executeAssigners(triggeredAssigns(symbol))
    }
  }

  def getTriggerExpressions: Iterable[Symbol] = {
    triggeredAssigns.keys
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

  /**
    * find the assigners downstream of the first order sensitivities of each
    * trigger then deduplicate and sort all assignments that depend on that trigger
    */
  def sortTriggeredAssigns(): Unit = {
    val unifiedTriggerAssigns = new mutable.HashMap[Symbol, mutable.ArrayBuffer[Assigner]]

    triggeredAssigns.foreach { case (trigger, assigners) =>
      val rootTrigger = {
        val parents = symbolTable.getSourcesOf(trigger)
        if(parents.isEmpty) {
          trigger
        }
        else {
          parents.minBy(_.cardinalNumber)
        }
      }
      if(unifiedTriggerAssigns.contains(rootTrigger)) {
        unifiedTriggerAssigns(rootTrigger) ++= assigners
      }
      else {
        unifiedTriggerAssigns(rootTrigger) = assigners
      }
    }

    triggeredAssigns.clear()

    unifiedTriggerAssigns.foreach { case (trigger, assigners) =>
      val sensitiveSymbols  = symbolTable.getChildren(Seq(trigger)).toSeq
      val senstiveAssigners = (assigners ++ symbolTable.getAssigners(sensitiveSymbols)).distinct

      triggeredAssigns(trigger) = senstiveAssigners.sortBy { assigner: Assigner =>
        assigner.symbol.cardinalNumber
      }
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
    }.mkString("\n") + "\n\n" +
    triggeredAssigns.keys.toList.map { key =>
      s"Triggered assigns for $key\n" +
      triggeredAssigns(key).map { assigner =>
        "  " + assigner.render
      }.mkString("\n")
    }.mkString("\n")
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
