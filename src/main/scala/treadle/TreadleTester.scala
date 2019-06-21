// See LICENSE for license details.

package treadle

import java.io.PrintWriter
import java.util.Calendar

import firrtl.{AnnotationSeq, ChirrtlForm, CircuitForm}
import firrtl.options.StageOptions
import firrtl.options.Viewer.view
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import treadle.chronometry.UTC
import treadle.executable._
import treadle.stage.{TreadleCompatibilityPhase, TreadleTesterPhase}

//TODO: Indirect assignments to external modules input is possibly not handled correctly
//TODO: Force values should work with multi-slot symbols

/**
  * Works a lot like the chisel classic tester compiles a firrtl input string
  * and allows poke, peek, expect and step
  *
  * pokes invalidate the underlying circuit
  * peek, expect and step, recompute (re-validate) the circuit before executing
  *
  * Important note: port names in LoFirrtl have replaced dot notation with underscore notation
  * so that io.a.b must be referenced as io_a_b
  *
  * @param annotationSeq   firrtl circuit and parameters for tester are to be found here
  */
//class TreadleTester(input: String, optionsManager: HasTreadleSuite = TreadleTester.getDefaultManager) {
class TreadleTester(annotationSeq: AnnotationSeq) {

  def this(input: String, optionsManager: HasTreadleSuite, circuitForm: CircuitForm = ChirrtlForm) = {
    this(TreadleCompatibilityPhase.checkFormTransform(circuitForm, optionsManager.toAnnotationSeq :+ FirrtlSourceAnnotation(input)))
  }

  var expectationsMet = 0

  treadle.random.setSeed(annotationSeq.collectFirst{ case RandomSeedAnnotation(seed) => seed }.getOrElse(0L))

  val wallTime = UTC()

  val engine         : ExecutionEngine  = ExecutionEngine(annotationSeq, wallTime)

  wallTime.onTimeChange = () => {
    engine.vcdOption.foreach { vcd =>
      vcd.setTime(wallTime.currentTime)}
  }

  val resetName: String = annotationSeq.collectFirst{ case ResetNameAnnotation(rn) => rn }.getOrElse("reset")
  private val clockInfo = annotationSeq.collectFirst{ case ClockInfoAnnotation(cia) => cia }.getOrElse(Seq.empty)
  private val writeVcd  = annotationSeq.exists { case WriteVcdAnnotation => true; case _ => false }
  val vcdShowUnderscored: Boolean  = annotationSeq.exists { case VcdShowUnderScoredAnnotation => true; case _ => false }
  private val callResetAtStartUp = annotationSeq.exists { case CallResetAtStartupAnnotation => true; case _ => false }
  private val topName = annotationSeq.collectFirst{ case OutputFileAnnotation(ofn) => ofn }.getOrElse(engine.ast.main)
  private val verbose  = annotationSeq.exists { case VerboseAnnotation => true; case _ => false }
  private val stageOptions = view[StageOptions](annotationSeq)


  def setVerbose(value: Boolean = true): Unit = {
    wallTime.isVerbose = value
    engine.setVerbose(value)
  }

  val startTime: Long = System.nanoTime()

  val clockInfoList: Seq[ClockInfo] = if(clockInfo.isEmpty) {
    if(engine.symbolTable.contains("clock")) {
      Seq(ClockInfo())
    }
    else if(engine.symbolTable.contains("clk")) {
      Seq(ClockInfo("clk"))
    }
    else {
      Seq()
    }
  }
  else {
    clockInfo
  }

  val clockStepper: ClockStepper = clockInfoList.length match {
    case 0 =>
      new NoClockStepper

    case 1 =>
      val clockInfo = clockInfoList.head
      wallTime.setTime(clockInfo.initialOffset)

      SimpleSingleClockStepper(
        engine,
        engine.dataStore,
        engine.symbolTable(clockInfo.name),
        engine.symbolTable.get(resetName),
        clockInfo.period,
        clockInfo.initialOffset,
        wallTime
      )
    case _ =>
      new MultiClockStepper(engine = this.engine, clockInfoList, wallTime)
  }

  /**
    * Advance time in ticks of the [[UTC]] wallTime, the default is picoseconds, but can be
    * read by the scaleName of the wallTime.  One should probably be advancing by some simple factor
    * of a clock period. The clockInfoList of the options should define this (could be more than one).
    * @param interval units are in units of the [[wallTime]] scale.
    */
  def advanceTime(interval: Long): Unit = {
    assert(interval >= 0L, "TreadleTester#advanceTime called with negative value")
    wallTime.setTime(wallTime.currentTime + interval)
  }

  /*
  The Idea here is that combinational delay will be used when a peek follows a poke without a step
  This should allow VCD output to show the events as if they had taken place in a small
  interval of the clock cycle. There is some DANGER here that an unusual test will poke then peek
  over 100 times before calling step, which will create a weird looking clock trace
   */
  val combinationalDelay: Long = {
    clockStepper match {
      case s: SimpleSingleClockStepper =>
        s.clockPeriod / 100
      case m: MultiClockStepper =>
        m.shortestPeriod / 100
      case _ =>
        0
    }
  }

  setVerbose(verbose)

  wallTime.setTime(0L)

  if(engine.verbose) {
    println(s"${"-"*60}\nStarting Treadle at ${Calendar.getInstance.getTime} WallTime: ${wallTime.currentTime}")
  }

  if(writeVcd) {
    engine.makeVCDLogger(
      stageOptions.getBuildFileName(topName, Some(".vcd")),
      vcdShowUnderscored
    )
  }

  if(callResetAtStartUp && engine.symbolTable.contains(resetName)) {
    clockInfoList.headOption.foreach { clockInfo =>
      reset(clockInfo.period + clockInfo.initialOffset)
    }
  }

  def reset(timeRaised: Long): Unit = {
    engine.symbolTable.get(resetName).foreach { resetSymbol =>
      engine.setValue(resetName, 1)

      clockStepper match {
        case _: NoClockStepper =>
          engine.setValue(resetName, 1)
          engine.evaluateCircuit()
          wallTime.incrementTime(timeRaised)
          engine.setValue(resetName, 0)
        case stepper: SimpleSingleClockStepper =>
          clockStepper.addTask(wallTime.currentTime + timeRaised + stepper.downPeriod) { () =>
            engine.setValue(resetName, 0)
            if (engine.verbose) {
              println(s"reset dropped at ${wallTime.currentTime}")
            }
            engine.evaluateCircuit()
          }
          while(engine.dataStore(resetSymbol) != Big0) {
            stepper.run(1)
          }

        case _ =>
          clockStepper.addTask(wallTime.currentTime + timeRaised) { () =>
            engine.setValue(resetName, 0)
            if (engine.verbose) {
              println(s"reset dropped at ${wallTime.currentTime}")
            }
          }
          wallTime.runUntil(wallTime.currentTime + timeRaised)
      }
    }
  }

  def makeSnapshot(): Unit = {
    val snapshotName = stageOptions.getBuildFileName(topName, Some(".datastore.snapshot.json"))
    val writer = new PrintWriter(snapshotName)
    writer.write(engine.dataStore.serialize)
    writer.close()
    println(s"Writing snapshot file $snapshotName")
  }

  /** Indicate a failure has occurred.  */
  private var failureTime = -1L
  private var failCode: Option[Int] = None
  def fail(code: Int): Unit = {
    if (failCode.isEmpty) {
      failureTime = System.nanoTime()
      failCode = Some(code)
      makeSnapshot()
    }
  }

  /** Indicate failure due to an exception.
    *
    * @param ex exception causing the failure
    * @param msg optional message to be printed
    */
  def fail(ex: Throwable, msg: Option[String ] = None): Nothing = {
    engine.writeVCD()

    msg match {
      case Some(s) => println(s)
      case _ =>
    }
    fail(2)
    throw ex
  }
  def isOK: Boolean = failCode match {
    case None | Some(0) => true
    case _ => false
  }

  def forceValue(name: String, value: BigInt): Unit = {
    engine.symbolTable.get(name) match {
      case Some(symbol) =>
        symbol.forcedValue = Some(value)
        if(engine.symbolTable.isRegister(name)) {
          engine.setValue(name, value, registerPoke = true)
        }
        engine.inputsChanged = true
      case _ => println(s"Error: forceValue($name, $value) $name not found in symbol table")
    }
    if(engine.dataStore.leanMode) {
      engine.scheduler.setLeanMode(false)
    }
  }

  def clearForceValue(name: String): Unit = {
    engine.symbolTable.get(name) match {
      case Some(symbol) =>
        symbol.forcedValue = None
        engine.inputsChanged = true
      case _ => println(s"Error: clearForceValue($name) $name not found in symbol table")
    }
  }

  /**
    * Pokes value to the port referenced by string
    * Warning: pokes to components other than input ports is currently
    * not supported but does not cause an error warning
    * This feature should be supported soon
    *
    * @param name the name of a port
    * @param value a value to put on that port
    */
  def poke(name: String, value: BigInt): Unit = {
    try {
      val isRegister = engine.symbolTable.isRegister(name)
      engine.setValue(name, value, registerPoke = isRegister)
    }
    catch {
      case ie: TreadleException =>
        fail(ie, Some(s"Error: poke($name, $value)"))
    }
  }

  /** inspect a value of a named circuit component
    *
    * @param name the name of a circuit component
    * @return A BigInt value currently set at name
    */
  def peek(name: String): BigInt = {
    if(engine.inputsChanged) {
      if(engine.verbose) {
        println(s"peeking $name on stale circuit, refreshing START")
      }
      engine.evaluateCircuit()
      clockStepper.combinationalBump(combinationalDelay)
      if(engine.verbose) {
        println(s"peeking $name on stale circuit, refreshing DONE")
      }
    }
    engine.getValue(name)
  }

  /**
    * require that a value be present on the named component
 *
    * @param name component name
    * @param expectedValue the BigInt value required
    */
  def expect(name: String, expectedValue: BigInt, message: String = ""): Unit = {
    val value = peek(name)
    if(value != expectedValue) {
      val info = engine.scheduler.getAssignerInfo(name)
      val renderer = new ExpressionViewRenderer(
        engine.dataStore, engine.symbolTable, engine.expressionViews)
      val calculation = renderer.render(engine.symbolTable(name), wallTime.currentTime)
      fail(
        TreadleException(s"Error:expect($name, $expectedValue) got $value $message\n$calculation\nAssigned at: $info")
      )
    }
    expectationsMet += 1
  }

  def cycleCount: Long = clockStepper.cycleCount

  /**
    * Cycles the circuit n steps (with a default of one)
    * At each step registers and memories are advanced and all other elements recomputed
    *
    * @param n cycles to perform
    */
  def step(n: Int = 1): Unit = {
    if(engine.verbose) println(s"In step at ${wallTime.currentTime}")
    clockStepper.run(n)
  }

  /**
    * Pokes value to the named memory at offset
    *
    * @param name  the name of a memory
    * @param index the offset in the memory
    * @param value a value to put on that port
    */
  def pokeMemory(name: String, index: Int, value: BigInt): Unit = {
    engine.symbolTable.get(name) match {
      case Some(_) =>
        engine.setValue(name, value = value, offset = index)
      case _ =>
        throw TreadleException(s"Error: memory $name.forceWrite($index, $value). memory not found")
    }
  }

  def peekMemory(name: String, index: Int): BigInt = {
    engine.symbolTable.get(name) match {
      case Some(_) =>
        engine.getValue(name, offset = index)
      case _ =>
        throw TreadleException(s"Error: get memory $name.forceWrite($index). memory not found")
    }
  }

  /**
    * require that a value be present on the named component
    *
    * @param name component name
    * @param expectedValue the BigInt value required
    */
  def expectMemory(name: String, index: Int, expectedValue: BigInt, message: String = ""): Unit = {
    val value = peekMemory(name, index)
    if(value != expectedValue) {
      val renderer = new ExpressionViewRenderer(
        engine.dataStore, engine.symbolTable, engine.expressionViews)
      val calculation = renderer.render(engine.symbolTable(name), wallTime.currentTime)
      fail(TreadleException (s"Error:expect($name, $expectedValue) got $value $message\n$calculation"))
    }
    expectationsMet += 1
  }

  def waveformValues(
      symbolNames: Array[String] = Array[String](),
      startCycle: Int = 0,
      endCycle: Int = -1
  ): WaveformValues = {
    val symbols = if (symbolNames.length == 0) {
      engine.symbolTable.nameToSymbol.values.toArray
    } else {
      val numSymbols = symbolNames.length
      new Array[Symbol](numSymbols)
    }

    if (symbolNames.length == 0) {
      symbolNames.zipWithIndex.foreach { case (symbolName, counter) =>
        assert(engine.symbolTable.contains(symbolName),
          s""""$symbolName" : argument is not an element of this circuit""")
        symbols.update(counter, engine.symbolTable(symbolName))
      }
    }

    engine.dataStore.getWaveformValues(symbols, startCycle, endCycle)
  }

  def allWaveformValues: WaveformValues = {
    engine.dataStore.getWaveformValues(engine.symbolTable.nameToSymbol.values.toArray)
  }

  def dependencyInfo(symbolName: String): String = engine.renderComputation(symbolName, showValues = false)

  def isRegister(symbolName: String): Boolean = engine.symbolTable.isRegister(symbolName)

  def reportString: String = {
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0
    /*
        This should not every show the Failed message because currently the engine
        throws an TreadleException on Stop (but maybe that will be made optional at some point)
        Best to leave this here for now, someone might catch the exception manually and still want to
        see this report which should include the Failed in that case
      */
    def status: String = {
      engine.lastStopResult match {
        case Some(stopResult) =>
          s"Failed: Stop result $stopResult:"
        case _ =>
          if (isOK) {
            s"Success:"
          } else {
            s"Failed: Code ${failCode.get}"
          }
      }
    }
    s"test ${engine.ast.main} " +
      s"$status $expectationsMet tests passed " +
      f"in $cycleCount cycles in $elapsedSeconds%.6f seconds ${cycleCount / elapsedSeconds}%.2f Hz"
  }
  /**
    * A simplistic report of the number of expects that passed
    */
  def report(): Unit = {
    engine.writeVCD()
    println(reportString)
  }

  def finish: Boolean = {
    engine.writeVCD()
    isOK
  }
}

object TreadleTester {
  /**
    * this convenience method avoids files laying around in current directory
    * @return
    */
  def getDefaultManager: HasTreadleSuite = {
    new TreadleOptionsManager {
      commonOptions = commonOptions.copy(targetDirName = "test_run_dir")
    }
  }

  /**
    * Create a treadle tester
    * @param input           the firrtl to parse
    * @param optionsManager  options manager
    * @return
    */
  @deprecated("Use TreadleTester(annotationSeq) instead")
  def apply(input : String, optionsManager: HasTreadleSuite = getDefaultManager): TreadleTester = {
    val sourceAnnotation = FirrtlSourceAnnotation(input)
    TreadleTester(sourceAnnotation +: optionsManager.toAnnotationSeq)
  }

  def apply(annotations: AnnotationSeq): TreadleTester = {
    val newAnnotations = TreadleTesterPhase.transform(annotations)
    newAnnotations.collectFirst { case TreadleTesterAnnotation(tester) => tester }.getOrElse(
      throw TreadleException(s"Could not create a TreadleTester")
    )
  }
}
