// SPDX-License-Identifier: Apache-2.0

package treadle.executable

import java.io.PrintWriter

import firrtl.annotations.ReferenceTarget
import firrtl.annotations.TargetToken.Instance
import firrtl.ir.{Circuit, NoInfo}
import firrtl.options.StageOptions
import firrtl.options.Viewer.view
import firrtl.{AnnotationSeq, MemKind, PortKind, RegKind}
import logger.LazyLogging
import treadle._
import treadle.blackboxes.PlusArg
import treadle.chronometry.{Timer, UTC}
import treadle.utils.{NameBasedRandomNumberGenerator, Render}
import treadle.vcd.VCD

import scala.collection.mutable

//scalastyle:off magic.number number.of.methods
class ExecutionEngine(
  val ast:             Circuit,
  val annotationSeq:   AnnotationSeq,
  val symbolTable:     SymbolTable,
  val dataStore:       DataStore,
  val scheduler:       Scheduler,
  val expressionViews: Map[Symbol, ExpressionView],
  val wallTime:        UTC)
    extends LazyLogging {
  val cycleTimeIncrement = 500

  var vcdOption:   Option[VCD] = None
  var vcdFileName: String = ""

  val expressionViewRenderer = new ExpressionViewRenderer(
    dataStore,
    symbolTable,
    expressionViews
  )

  scheduler.executionEngineOpt = Some(this)

  val symbolsPokedSinceEvaluation: mutable.HashSet[Symbol] = new mutable.HashSet

  var verbose: Boolean = false
  setVerbose(annotationSeq.exists { case VerboseAnnotation => true; case _ => false })

  val userRandomSeed: Long = annotationSeq.collectFirst { case RandomSeedAnnotation(seed) => seed }.getOrElse(0L)

  var inputsChanged: Boolean = false

  /* Default dataStore plugins */

  dataStore.addPlugin(
    "show-assigns",
    new ReportAssignments(this),
    enable = verbose
  )

  val symbolsToWatch: Seq[String] = annotationSeq.collectFirst { case SymbolsToWatchAnnotation(stw) => stw }.getOrElse {
    Seq.empty
  }

  dataStore.addPlugin(
    "show-computation",
    new RenderComputations(this, symbolsToWatch),
    enable = symbolsToWatch.nonEmpty
  )

  annotationSeq.collect { case a: DataStorePlugInAnnotation => a }.foreach { a =>
    dataStore.addPlugin(a.name, a.getPlugin(this), enable = true)
  }

  def setLeanMode(): Unit = {
    val canBeLean = !(verbose || dataStore.hasEnabledPlugins)
    scheduler.setLeanMode(canBeLean)
    scheduler.setVerboseAssign(verbose)
  }

  /**
    * turns on evaluator debugging.  Can make output quite
    * verbose.
    *
    * @param isVerbose  The desired verbose setting
    */
  def setVerbose(isVerbose: Boolean = true): Unit = {
    verbose = isVerbose
    setLeanMode()
    dataStore.plugins.get("show-assigns") match {
      case Some(plugin) => plugin.setEnabled(verbose)
      case _            => None
    }
    scheduler.setVerboseAssign(isVerbose)
  }

  val timer = new Timer

  if (annotationSeq.contains { RandomizeAtStartupAnnotation }) {
    randomize()
  }

  if (verbose) {
    if (scheduler.orphanedAssigns.nonEmpty) {
      Render.headerBar(s"Executing static assignments", offset = 8)
    } else {
      Render.headerBar(s"No static assignments", offset = 8)
    }
  }
  scheduler.executeOrphanedAssigns()
  if (verbose) {
    if (scheduler.orphanedAssigns.nonEmpty) {
      Render.headerBar(s"Finished executing static assignments", offset = 8)
    }
  }

  val memoryInitializer = new MemoryInitializer(this)

  def makeVCDLogger(
    fileName:        String,
    showUnderscored: Boolean,
    memoryLogger:    VcdMemoryLoggingController = new VcdMemoryLoggingController()
  ): Unit = {
    val vcd = VCD(ast.main, showUnderscoredNames = showUnderscored)

    symbolTable.instanceNames.foreach { name =>
      vcd.scopeRoot.addScope(name)
    }
    vcd.timeStamp = -1
    symbolTable.symbols.foreach { symbol =>
      vcd.wireChanged(symbol.name, dataStore(symbol), symbol.bitWidth)
      if (symbol.dataKind == MemKind) {
        memoryLogger.getIndexedNames(symbol).foreach { indexedMemName =>
          vcd.wireChanged(indexedMemName, 0, symbol.bitWidth)
        }
      }
    }
    vcd.timeStamp = 0

    vcdOption = Some(vcd)
    vcdFileName = fileName

    val vcdPlugIn = new VcdHook(this, memoryLogger)
    dataStore.addPlugin(ExecutionEngine.VCDHookName, vcdPlugIn, enable = true)
  }

  def disableVCD(): Unit = {
    writeVCD()
    vcdOption = None
    vcdFileName = ""
    dataStore.removePlugin(ExecutionEngine.VCDHookName)
  }

  def writeVCD(): Unit = {
    vcdOption.foreach { vcd =>
      vcd.write(vcdFileName)
    }
  }

  /** Randomize the circuits registers and memories
    *
    * @param additonalSeed a seed to move change all the random numbers generated
    */
  def randomize(additonalSeed: Long = 0L): Unit = {
    val randomGenerator = new NameBasedRandomNumberGenerator

    val symbolsToDo: Seq[Symbol] = symbolTable.symbols.toSeq

    symbolsToDo.foreach { symbol =>
      def getRandomValue: BigInt = {
        val big = randomGenerator.nextBigInt(symbol.name, userRandomSeed + additonalSeed, symbol.bitWidth)
        val newValue = if (symbol.dataType == SignedInt) {
          symbol.makeSInt(big, symbol.bitWidth)
        } else {
          symbol.makeUInt(big, symbol.bitWidth)
        }
        newValue
      }

      if (symbol.dataKind == RegKind) {
        val newValue = getRandomValue
        logger.info(s"setting ${symbol.name} <= $newValue")
        setValue(symbol.name, getRandomValue)
      } else if (symbol.dataKind == MemKind) {
        for (slot <- 0 until symbol.slots) {
          val newValue = getRandomValue
          logger.info(s"setting ${symbol.name}($slot) <= $newValue")
          setValue(symbol.name, getRandomValue, offset = slot)
        }
      }

    }
    evaluateCircuit()
  }

  def renderComputation(symbolNames: String, outputFormat: String = "d", showValues: Boolean = true): String = {
    val renderer = new ExpressionViewRenderer(dataStore, symbolTable, expressionViews)

    val symbols = symbolNames
      .split(",")
      .map(_.trim)
      .flatMap { s =>
        symbolTable.get(s)
      }
      .distinct

    symbols.flatMap { symbol =>
      expressionViews.get(symbol) match {
        case Some(_) =>
          Some(s"${renderer.render(symbol, wallTime.currentTime, outputFormat = outputFormat, showValues)}")
        case _ => None
      }
    }.mkString("\n")
  }

  private def runAssigns(): Unit = {
    try {
      scheduler.executeCombinationalAssigns()

      // save data state under roll back buffers if they are being used
      dataStore.saveData(wallTime.currentTime)

      if (lastStopResult.isDefined) {
        writeVCD()
        val stopKind = if (lastStopResult.get > 0) { "Failure Stop" }
        else { "Stopped" }
        throw StopException(s"$stopKind: result ${lastStopResult.get}")
      }
    } catch {
      case throwable: Throwable =>
        writeVCD()
        throw throwable
    }
  }

  def getValue(name: String, offset: Int = 0): BigInt = {
    assert(symbolTable.contains(name), s"""Error: getValue("$name") : argument is not an element of this circuit""")

    if (inputsChanged) {
      if (verbose) {
        Render.headerBar(s"peeking", offset = 8)
      }
      inputsChanged = false
      runAssigns()
    }

    val symbol = symbolTable(name)
    if (offset == 0) {
      symbol.normalize(dataStore(symbol))
    } else {
      if (offset - 1 > symbol.slots) {
        throw TreadleException(s"get value from ${symbol.name} offset $offset > than size ${symbol.slots}")
      }
      symbol.normalize(dataStore.getValueAtIndex(symbol.dataSize, index = symbol.index + offset))
    }
  }

  /**
    * Update the dataStore with the supplied information.
    * IMPORTANT: This should never be used internally.
    *
    * @param name  name of value to set
    * @param value new concrete value
    * @param force allows setting components other than top level inputs
    * @param registerPoke changes which side of a register is poked
    * @return the concrete value that was derived from type and value
    */
  // scalastyle:off cyclomatic.complexity method.length
  def setValue(
    name:         String,
    value:        BigInt,
    force:        Boolean = true,
    registerPoke: Boolean = false,
    offset:       Int = 0
  ): BigInt = {

    val symbol = symbolTable.getOrElse(
      name,
      throw TreadleException(s"setValue: Cannot find $name in symbol table")
    )

    inputsChanged = true
    if (symbolsPokedSinceEvaluation.contains(symbol)) {
      if (verbose) {
        println(s"updating circuit on second update of same input without clock advance")
      }
      symbolsPokedSinceEvaluation.clear()
      scheduler.executeCombinationalAssigns()
    } else {
      symbolsPokedSinceEvaluation += symbol
    }

    if (!force) {
      assert(
        symbol.dataKind == PortKind,
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override"
      )
      return Big0
    }

    val adjustedValue = symbol.valueFrom(value)
    if (offset == 0) {
      if (verbose) {
        if (!inputsChanged) {
          Render.headerBar("Poking")
        }
        println(s"${symbol.name} <= $value")
      }
      dataStore.update(symbol, adjustedValue)
      vcdOption.foreach { vcd =>
        vcd.wireChanged(symbol.name, adjustedValue, symbol.bitWidth)
      }
    } else {
      if (offset - 1 > symbol.slots) {
        throw TreadleException(s"get value from ${symbol.name} offset $offset > than size ${symbol.slots}")
      }
      if (verbose) {
        if (!inputsChanged) {
          Render.headerBar("Poking")
        }

        println(s"${symbol.name}($offset) <= $value from tester")
      }
      dataStore.setValueAtIndex(symbol.dataSize, symbol.index + offset, value)
    }

    value
  }

  /**
    * Update the dataStore with the supplied information.
    * IMPORTANT: This should never be used internally.
    *
    * @param symbol symbol to set
    * @param value new concrete value
    */
  // scalastyle:off cyclomatic.complexity method.length
  def setIntValue(
    symbol: Symbol,
    value:  Int
  ): Int = {

    inputsChanged = true
    if (symbolsPokedSinceEvaluation.contains(symbol)) {
      if (verbose) {
        println(s"updating circuit on second update of same input without clock advance")
      }
      symbolsPokedSinceEvaluation.clear()
      scheduler.executeCombinationalAssigns()
    } else {
      symbolsPokedSinceEvaluation += symbol
    }

    val adjustedValue = symbol.valueFrom(value)
    if (verbose) {
      if (!inputsChanged) {
        Render.headerBar("Poking")
      }
      println(s"${symbol.name} <= $value")
    }
    dataStore.intData(symbol.index) = value
    vcdOption.foreach { vcd =>
      vcd.wireChanged(symbol.name, adjustedValue, symbol.bitWidth)
    }

    value
  }

  def isRegister(name: String): Boolean = {
    symbolTable.registerNames.contains(name)
  }

  def getRegisterNames: Seq[String] = {
    symbolTable.registerNames.toSeq
  }

  def getInputPorts: Seq[String] = {
    symbolTable.inputPortsNames.toSeq
  }

  def getOutputPorts: Seq[String] = {
    symbolTable.outputPortsNames.toSeq
  }

  def isInputPort(name: String): Boolean = {
    symbolTable.inputPortsNames.contains(name)
  }

  def isOutputPort(name: String): Boolean = {
    symbolTable.outputPortsNames.contains(name)
  }

  def validNames: Iterable[String] = symbolTable.keys
  def symbols:    Iterable[Symbol] = symbolTable.symbols

  /** returns all the symbols identified by the provided referenceTarget
    *
    * @param referenceTarget identifies a symbol or symbols
    * @return
    */
  def referenceTargetToSymbols(referenceTarget: ReferenceTarget): Seq[Symbol] = {
    if (referenceTarget.path.nonEmpty) {
      // a specific path into the circuit
      val pathName = referenceTarget.path.map { case (Instance(name), _) => name }.mkString(".")
      val symbols = symbolTable.instanceNameToModuleName.flatMap {
        case (instanceName, _) if instanceName.endsWith(pathName) =>
          val symbolName = s"$instanceName.${referenceTarget.ref}"
          symbolTable.get(symbolName)
        case _ => None
      }.toSeq
      symbols
    } else if (referenceTarget.module == ast.main) {
      // top level reference
      symbolTable.get(referenceTarget.ref) match {
        case Some(symbol) => Seq(symbol)
        case _            => Seq.empty
      }
    } else if (referenceTarget.module != ast.main) {
      // module level reference
      val targetModule = s"${referenceTarget.module}"
      symbolTable.instanceNameToModuleName.flatMap {
        case (instance, moduleName) if moduleName == targetModule =>
          val name = s"$instance.${referenceTarget.ref}"
          symbolTable.get(name)
        case _ => None
      }.toSeq
    } else {
      Seq.empty
    }
  }

  def evaluateCircuit(): Unit = {
    if (inputsChanged) {
      inputsChanged = false
      symbolsPokedSinceEvaluation.clear()

      if (verbose) {
        Render.headerBar(s"combinational evaluate", offset = 8)
      }
      runAssigns()

      if (verbose) {
        Render.headerBar(s"done combinational evaluate", offset = 8)
      }

    }
  }

  def advanceTime(increment: Long): Unit = {
    if (increment > 0) {
      if (inputsChanged) {
        evaluateCircuit()
      }
//      wallTime.advance(increment)
    }
  }

  private val stopHappenedSymbolOpt = symbolTable.get(StopOp.stopHappenedName)

  /**
    * returns that value specified by a StopOp when
    * its condition is satisfied.  Only defined when
    * circuit is currently stopped.
    * @return
    */
  def lastStopResult: Option[Int] = {
    stopHappenedSymbolOpt match {
      case Some(hasStoppedSymbol) =>
        val stopValue = dataStore(hasStoppedSymbol).toInt
        if (stopValue > 0) {
          Some(stopValue - 1)
        } else {
          None
        }
      case _ =>
        None
    }
  }

  /**
    * Is the circuit currently stopped.  StopOp throws a
    * Stop
    * @return
    */
  def stopped: Boolean = {
    lastStopResult.isDefined
  }

  def finish(): Unit = {
    symbols.foreach { symbol =>
      symbolTable.getBlackboxImplementation(symbol).foreach { blackBox =>
        blackBox.finish()
      }
    }
  }

  def fieldsHeader: String = {
    "Buf " +
      symbolTable.keys.toArray.sorted.map { name =>
        val s = name.takeRight(9)
        f"$s%10.10s"
      }.mkString("")
  }

  def header: String = {
    fieldsHeader
  }

  def dataInColumns: String = {
    val keys = symbolTable.keys.toArray.sorted

    ("-" * fieldsHeader.length) + "\n" +
      keys.map { name =>
        val symbol = symbolTable(name)
        val value = symbol.normalize(dataStore(symbolTable(name)))
        f" $value%9.9s"
      }.mkString("") + "\n" +
      ("-" * fieldsHeader.length)
  }

  def getInfoString: String = "Info" //TODO (chick) flesh this out
  def getPrettyString: String = {
    header + "\n" +
      dataInColumns
  }

  trait ClockToggle {
    def raiseClock(): Unit = {}
    def lowerClock(): Unit = {}
  }

  class NullToggler extends ClockToggle

  def makeUpToggler(symbol: Symbol): Assigner = {
    val assigner = dataStore.AssignInt(symbol, GetIntConstant(1).apply _, NoInfo)

    if (vcdOption.isDefined) assigner.setLeanMode(false)
    assigner.setVerbose(verbose)
    assigner
  }

  def makeDownToggler(symbol: Symbol): Assigner = {
    val assigner = dataStore.AssignInt(symbol, GetIntConstant(0).apply _, NoInfo)

    if (vcdOption.isDefined) assigner.setLeanMode(false)
    assigner.setVerbose(verbose)
    assigner
  }
}

object ExecutionEngine extends LazyLogging {

  val VCDHookName = "log-vcd"

  //scalastyle:off method.length
  /**
    * Factory to create an execution engine
    * @param annotationSeq  annotations control all
    * @param wallTime       external synthetic simple time
    * @return
    */
  def apply(annotationSeq: AnnotationSeq, wallTime: UTC): ExecutionEngine = {
    val timer = new Timer
    val t0 = System.nanoTime()
    val stageOptions = view[StageOptions](annotationSeq)

    val circuit = annotationSeq.collectFirst { case TreadleCircuitStateAnnotation(c) => c }.get.circuit

    if (annotationSeq.contains(ShowFirrtlAtLoadAnnotation)) {
      println(circuit.serialize)
    }

    if (annotationSeq.contains(SaveFirrtlAtLoadAnnotation)) {
      val fileName = stageOptions.getBuildFileName(circuit.main, Some(".treadle.lo.fir"))
      val writer = new PrintWriter(fileName)
      writer.println(circuit.serialize)
      writer.close()
    }

    val blackBoxFactories = annotationSeq.flatMap {
      case BlackBoxFactoriesAnnotation(bbf) => bbf
      case _                                => Seq.empty
    }
    val allowCycles = annotationSeq.exists { case AllowCyclesAnnotation => true; case _ => false }
    val prefixPrintfWithTime = annotationSeq.exists { case PrefixPrintfWithWallTime => true; case _ => false }

    val rollbackBuffers = annotationSeq.collectFirst { case RollBackBuffersAnnotation(rbb) => rbb }.getOrElse(
      TreadleDefaults.RollbackBuffers
    )
    val plusArgs = annotationSeq.collectFirst { case PlusArgsAnnotation(seq) => seq }
      .getOrElse(Seq.empty)
      .map { s =>
        PlusArg(s)
      }

    val validIfIsRandom = annotationSeq.exists { case ValidIfIsRandomAnnotation => true; case _ => false }
    val verbose = annotationSeq.exists { case VerboseAnnotation => true; case _ => false }

    val symbolTable: SymbolTable = timer("Build Symbol Table") {
      SymbolTable(circuit, blackBoxFactories, allowCycles)
    }

    val dataStoreAllocator = new DataStoreAllocator

    symbolTable.allocateData(dataStoreAllocator)

    val dataStore = DataStore(rollbackBuffers, dataStoreAllocator)

    if (verbose) {
      println(s"Symbol table:\n${symbolTable.render}")
    }

    val scheduler = new Scheduler(symbolTable)

    val compiler = new ExpressionCompiler(
      symbolTable,
      dataStore,
      scheduler,
      validIfIsRandom,
      prefixPrintfWithTime,
      blackBoxFactories,
      plusArgs
    )

    timer("Build Compiled Expressions") {
      compiler.compile(circuit, blackBoxFactories)
    }

    val expressionViews: Map[Symbol, ExpressionView] = ExpressionViewBuilder.getExpressionViews(
      symbolTable,
      dataStore,
      scheduler,
      validIfIsRandom,
      circuit,
      blackBoxFactories
    )

    scheduler.organizeAssigners()

    val executionEngine =
      new ExecutionEngine(circuit, annotationSeq, symbolTable, dataStore, scheduler, expressionViews, wallTime)

    executionEngine.dataStore.setExecutionEngine(executionEngine)

    if (verbose) {
      println(s"\n${scheduler.render(executionEngine)}")
      scheduler.setVerboseAssign(verbose)
    }

    // Do this to make sure inits and what-not happen
    executionEngine.inputsChanged = true

    val t1 = System.nanoTime()
    val total_seconds = (t1 - t0).toDouble / Timer.TenTo9th
    logger.info(
      s"file loaded in $total_seconds seconds, ${symbolTable.size} symbols, " +
        s"${scheduler.combinationalAssigns.size} statements"
    )

    executionEngine.memoryInitializer.initializeMemoriesFromFiles()

    executionEngine
  }
}
