// See LICENSE for license details.

package treadle

import firrtl.PortKind
import firrtl.ir.Circuit
import treadle.executable._
import treadle.vcd.VCD

//scalastyle:off magic.number
class ExecutionEngine(
    val ast: Circuit,
    val optionsManager: HasInterpreterSuite,
    val symbolTable: SymbolTable,
    val dataStore: DataStore,
    val scheduler: Scheduler,
    val expressionViews: Map[Symbol, ExpressionView]
) {
  private val interpreterOptions: TreadleOptions = optionsManager.treadleOptions

  var vcdOption: Option[VCD] = None
  var vcdFileName: String = ""
  var verbose: Boolean = interpreterOptions.setVerbose
  var inputsChanged: Boolean = false

  def setLeanMode(): Unit = {
    val canBeLean = ! (verbose || vcdOption.isDefined)
    scheduler.setLeanMode(canBeLean)
    scheduler.setVerboseAssign(verbose)
  }

  /**
    * turns on evaluator debugging. Can make output quite
    * verbose.
    *
    * @param isVerbose  The desired verbose setting
    */
  def setVerbose(isVerbose: Boolean = true): Unit = {
    setLeanMode()
    scheduler.setVerboseAssign(isVerbose)
    verbose = isVerbose
  }

  val timer = new Timer

  val clockOption: Option[Symbol] = {
    symbolTable.get("clock") match {
      case Some(clock) => Some(clock)
      case _           => symbolTable.get("clk")
    }
  }

  if(verbose) {
    println(s"Executing static assignments")
  }
  scheduler.executeAssigners(scheduler.orphanedAssigns)
  if(verbose) {
    println(s"Finished executing static assignments")
  }

  /**
    * Once a stop has occured, the engine will not allow pokes until
    * the stop has been cleared
    */
  def clearStop(): Unit = {dataStore(StopOp.StopOpSymbol) = 0}

  def makeVCDLogger(fileName: String, showUnderscored: Boolean): Unit = {
    val vcd = VCD(ast.main)

    symbolTable.instanceNames.foreach { name =>
      vcd.scopeRoot.addScope(name)
    }
    vcd.timeStamp = -1
    symbolTable.symbols.foreach { symbol =>
      vcd.wireChanged(symbol.name, dataStore(symbol), symbol.bitWidth)
    }
    vcd.timeStamp = 0

    vcdOption = Some(vcd)
    vcdFileName = fileName

    dataStore.vcdOption = vcdOption

    setLeanMode()
  }
  def disableVCD(): Unit = {
    writeVCD()
    vcdOption = None
    vcdFileName = ""
    setLeanMode()
  }
  def writeVCD(): Unit = {
    vcdOption.foreach(_.write(vcdFileName))
  }

  setVerbose(interpreterOptions.setVerbose)

  var isStale: Boolean = false

  def renderComputation(symbolNames: String): String = {
    val renderer = new ExpressionViewRenderer(dataStore, symbolTable, expressionViews)

    val symbols = symbolNames.split(",").map(_.trim).flatMap { s => symbolTable.get(s) }.distinct

    /* this forces the circuit to be current */
    scheduler.executeInputSensitivities()

    symbols.flatMap { symbol =>
      expressionViews.get(symbol) match {
        case Some(_) =>
          Some(s"${renderer.render(symbol)}")
        case _ => None
      }
    }.mkString("\n")
  }

  def getValue(name: String, offset: Int = 0): BigInt = {
    assert(symbolTable.contains(name),
      s"Error: getValue($name) is not an element of this circuit")

    if(inputsChanged) {
      if(verbose) {
        println(s"Executing assigns that depend on inputs")
      }
      inputsChanged = false
      scheduler.executeInputSensitivities()
    }

    val symbol = symbolTable(name)
    if(offset == 0) {
      symbol.normalize(dataStore(symbol))
    }
    else {
      if(offset - 1 > symbol.slots) {
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
  def setValue(
                name:         String,
                value:        BigInt,
                force:        Boolean = true,
                registerPoke: Boolean = false,
                offset:        Int = 0
              ): BigInt = {
    if(! symbolTable.contains(name)) {
      throw TreadleException(s"setValue: Cannot find $name in symbol table")
    }
    val symbol = symbolTable(name)

    inputsChanged = true

    if(!force) {
      assert(symbol.dataKind == PortKind,
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      if(checkStopped("setValue")) return Big0
    }

    val adjustedValue = symbol.valueFrom(value)
    if(offset == 0) {
      if(verbose) {
        println(s"${symbol.name} <= $value")
      }
      dataStore(symbol) = adjustedValue
    }
    else {
      if(offset - 1 > symbol.slots) {
        throw TreadleException(s"get value from ${symbol.name} offset $offset > than size ${symbol.slots}")
      }
      if(verbose) {
        println(s"${symbol.name}($offset) <= $value")
      }
      dataStore.setValueAtIndex(symbol.dataSize, symbol.index + offset, value)
    }

    if(! symbolTable.isTopLevelInput(name)) {
      val sensitiveSignals = symbolTable.childrenOf.reachableFrom(symbolTable(name)).toSeq
      val sensitiveAssigners = symbolTable.getAssigners(sensitiveSignals)
      scheduler.executeAssigners(sensitiveAssigners)
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
  def symbols: Iterable[Symbol] = symbolTable.symbols

  def evaluateCircuit(specificDependencies: Seq[String] = Seq()): Unit = {
    dataStore.advanceBuffers()

    if(verbose) {
      println("Inputs" + ("-" * 120))

      symbolTable.inputPortsNames.map(symbolTable(_)).foreach { symbol =>
        println(s"${symbol.name} is ${dataStore(symbol)} ")
      }
      println("-" * 120)
    }

    scheduler.getTriggerExpressions.foreach { key =>
      if(verbose) {
        println(s"Running triggered expressions for $key")
      }
      scheduler.executeTriggeredAssigns(key)
    }

    if(verbose) {
      println(s"Executing assigns that depend on inputs")
    }
    if(inputsChanged) {
      inputsChanged = false
      scheduler.executeInputSensitivities()
    }
    if (stopped) {
      lastStopResult match {
        case Some(0) =>
          throw StopException(s"Success: Stop result 0")
        case Some(errorResult) =>
          throw StopException(s"Failure: Stop result $errorResult")
        case result =>
          throw StopException(s"Failure: Stop with unexpected result $result")
      }
    }
  }

  def checkStopped(attemptedCommand: => String = "command"): Boolean = {
    if(stopped) {
      println(s"circuit has been stopped: ignoring $attemptedCommand")
    }
    stopped
  }

  def cycle(showState: Boolean = false): Unit = {
    if(checkStopped("cycle")) return

    if(inputsChanged) {
      if(verbose) {
        println(s"Executing assigns that depend on inputs")
      }
      inputsChanged = false
      scheduler.executeInputSensitivities()
    }

    clockOption.foreach { clock =>
      vcdOption.foreach(_.raiseClock())
      dataStore.AssignInt(clock, GetIntConstant(1).apply).run()
    }
    evaluateCircuit()
    clockOption.foreach { clock =>
      vcdOption.foreach(_.lowerClock())
      dataStore.AssignInt(clock, GetIntConstant(0).apply).run()
    }

    if(showState) println(s"ExecutionEngine: next state computed ${"="*80}\n$dataInColumns")
  }

  def doCycles(n: Int): Unit = {
    if(checkStopped(s"doCycles($n)")) return

    println(s"Initial state ${"-"*80}\n$dataInColumns")

    for(cycle_number <- 1 to n) {
      println(s"Cycle $cycle_number ${"-"*80}")
      cycle()
      if(stopped) return
    }
  }

  /**
    * returns that value specified by a StopOp when
    * its condition is satisfied.  Only defined when
    * circuit is currently stopped.
    * @return
    */
  def lastStopResult: Option[Int] = {
    if(symbolTable.contains(StopOp.StopOpSymbol.name)) {
      val stopValue = dataStore(StopOp.StopOpSymbol).toInt
      if (stopValue > 0) {
        Some(stopValue - 1)
      }
      else {
        None
      }
    }
    else {
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

  def header: String = {
    "Buf " +
      symbolTable.keys.toArray.sorted.map { name =>
        val s = name.takeRight(9)
        f"$s%10.10s"
      }.mkString("")
  }

  def dataInColumns: String = {
    val keys = symbolTable.keys.toArray.sorted
    ("-" * 100) + f"\n${dataStore.previousBufferIndex}%2s  " +
      keys.map { name =>
        val symbol = symbolTable(name)
        val value = symbol.normalize(dataStore.earlierValue(symbolTable(name), 1))
        f"$value%10.10s" }.mkString("") + f"\n${dataStore.currentBufferIndex}%2s  " +
      keys.map { name =>
        val symbol = symbolTable(name)
        val value = symbol.normalize(dataStore(symbolTable(name)))
        f"$value%10.10s" }.mkString("") + "\n" +
      ("-" * 100)
  }

  def getInfoString: String = "Info"  //TODO (chick) flesh this out
  def getPrettyString: String = {
    header + "\n" +
    dataInColumns
  }
}

object ExecutionEngine {
  //scalastyle:off method.length
  /**
    * Construct a Firrtl Execution engine
    * @param input           a Firrtl text file
    * @param optionsManager  options that control configuration and behavior
    * @return                the constructed engine
    */
  def apply(input: String, optionsManager: HasInterpreterSuite = new InterpreterOptionsManager): ExecutionEngine = {
    val interpreterOptions: TreadleOptions = optionsManager.treadleOptions

    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val verbose: Boolean = interpreterOptions.setVerbose
    val blackBoxFactories: Seq[BlackBoxFactory] = interpreterOptions.blackBoxFactories
    val timer = new Timer

    val loweredAst: Circuit = if(interpreterOptions.lowCompileAtLoad) {
      ToLoFirrtl.lower(ast, optionsManager)
    } else {
      ast
    }

    if(interpreterOptions.showFirrtlAtLoad) {
      println("LoFirrtl" + "=" * 120)
      println(loweredAst.serialize)
    }

    val symbolTable: SymbolTable = timer("Build Symbol Table") {
      SymbolTable(loweredAst, blackBoxFactories, interpreterOptions.allowCycles)
    }

    val dataStore = DataStore(
      numberOfBuffers = interpreterOptions.rollbackBuffers + 1, optimizationLevel = if (verbose) 0 else 1)

    symbolTable.allocateData(dataStore)
    println(s"Symbol table:\n${symbolTable.render}")

    val scheduler = new Scheduler(dataStore, symbolTable)

    val compiler = new ExpressionCompiler(symbolTable, dataStore, scheduler, interpreterOptions, blackBoxFactories)

    timer("Build Compiled Expressions") {
      compiler.compile(loweredAst, blackBoxFactories)
    }

    val expressionViews: Map[Symbol, ExpressionView] = ExpressionViewBuilder.getExpressionViews(
      symbolTable, dataStore, scheduler,
      interpreterOptions.validIfIsRandom,
      loweredAst, blackBoxFactories)

    val orphansAndSensitives = symbolTable.orphans ++ symbolTable.getChildren(symbolTable.orphans)

    scheduler.setOrphanedAssigners(symbolTable.getAssigners(orphansAndSensitives))

    // println(s"Scheduler before sort ${scheduler.renderHeader}")
    scheduler.inputDependentAssigns ++= symbolTable.inputChildrenAssigners()
    scheduler.sortInputSensitiveAssigns()
    scheduler.sortTriggeredAssigns()

    if(verbose) {
      println(s"\n${scheduler.render}")
    }

    new ExecutionEngine(ast, optionsManager, symbolTable, dataStore, scheduler, expressionViews)
  }
}
