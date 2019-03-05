// See LICENSE for license details.

package treadle.executable

import firrtl.ir.{Info, NoInfo}
import logger.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * The scheduler holds the assignment statements of the entire circuit.
  * Clocks introduce a level of complexity, in that when they flip registers
  * who use those clocks must copy there inputs to their outputs.
  * Also since registers are triggered by the derived highest level clock
  * that can be found, child clocks must be separately driven.  This is
  * what the triggeredUnassigns are for.
  *
  * @param symbolTable symbol table is used to find orphans
  */
class Scheduler(val symbolTable: SymbolTable) extends LazyLogging {

  var combinationalAssigns : mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val endOfCycleAssigns    : mutable.HashSet[Assigner] = new mutable.HashSet

  val registerClocks       : mutable.HashSet[Symbol] = new mutable.HashSet

  val triggeredUnassigns     : mutable.HashMap[Symbol,mutable.ArrayBuffer[Assigner]] =
    new mutable.HashMap[Symbol,mutable.ArrayBuffer[Assigner]] {
      override def default(key: Symbol): ArrayBuffer[Assigner] = {
        this(key) = new mutable.ArrayBuffer[Assigner]()
        this(key)
      }
    }

  val orphanedAssigns   : mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  private val toAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()

  def addAssigner(
    symbol: Symbol,
    assigner: Assigner,
    excludeFromCombinational: Boolean = false
  ): Unit = {

    toAssigner(symbol) = assigner
    combinationalAssigns += assigner
  }

  def addEndOfCycleAssigner(assigner: Assigner): Unit = {
    if(! endOfCycleAssigns.exists(a => a.symbol == assigner.symbol)) {
      endOfCycleAssigns += assigner
    }
  }

  def hasAssigner(symbol: Symbol): Boolean = {
    toAssigner.contains(symbol)
  }

  def getAllAssigners: Seq[Assigner] = {
    toAssigner.values.toSeq
  }

  def getAssignerInfo(symbol: Symbol): Info = {
    getAllAssigners.find(assigner => assigner.symbol == symbol) match {
      case Some(assigner) => assigner.info
      case _ => NoInfo
    }
  }

  def getAssignerInfo(symbolName: String): Info = {
    symbolTable.get(symbolName) match {
      case Some(symbol) => getAssignerInfo(symbol)
      case _ => NoInfo
    }
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
    val orphansAndSensitives = symbolTable.orphans.flatMap(s => toAssigner.get(s)).flatMap {
      case _: BlackBoxCycler => None
      case _: StopOp         => None
      case _: PrintfOp       => None
      case assigner          => Some(assigner)
    }

    setOrphanedAssigners(orphansAndSensitives)
    sortInputSensitiveAssigns()
  }

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setVerbose(isVerbose)
    }
    getAllAssigners.foreach { setMode }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    getAllAssigners.foreach { setMode }
  }

  /**
    * Execute the seq of assigners
    * @param assigners list of assigners
    */
  private def executeAssigners(assigners: Seq[Assigner]): Unit = {
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
  def executeCombinationalAssigns(): Unit = {
    executeAssigners(combinationalAssigns)
  }

  /**
    *  updates signals that depend on inputs
    */
  def executeOrphanedAssigns(): Unit = {
    executeAssigners(orphanedAssigns)
  }

  /**
    * de-duplicates and sorts assignments that depend on top level inputs.
    */
  def sortInputSensitiveAssigns(): Unit = {
    val deduplicatedAssigns = (combinationalAssigns -- orphanedAssigns).distinct
    combinationalAssigns = deduplicatedAssigns.sortBy { assigner: Assigner =>
      assigner.symbol.cardinalNumber
    } ++ endOfCycleAssigns
  }

  def setOrphanedAssigners(assigners: Seq[Assigner]): Unit = {
    orphanedAssigns.clear()
    orphanedAssigns ++= assigners
  }

  /**
    * Render the assigners managed by this scheduler
    * @return
    */
  def render(engine: ExecutionEngine): String = {
    val expressionViewRenderer = new ExpressionViewRenderer(
      engine.dataStore,
      symbolTable,
      engine.expressionViews,
      maxDependencyDepth = 0
    )

    def renderAssigner(assigner: Assigner): String = {
      val expression =
        expressionViewRenderer.render(assigner.symbol, engine.wallTime.currentTime, showValues = false)

      if(expression.isEmpty) {
        s"${assigner.symbol.name} :::"
      }
      else {
        s"${expression.toString}"
      }
    }

    s"Static assigns (${orphanedAssigns.size})\n" +
      orphanedAssigns.map(renderAssigner).mkString("\n") + "\n\n" +
    s"Active assigns (${combinationalAssigns.size})\n" +
    combinationalAssigns.map(renderAssigner).mkString("\n") + "\n\n"
  }
}

object Scheduler {
  def apply(symbolTable: SymbolTable): Scheduler = new Scheduler(symbolTable)
}
