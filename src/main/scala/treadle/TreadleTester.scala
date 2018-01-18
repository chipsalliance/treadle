// See LICENSE for license details.
package treadle

import java.io.PrintWriter

import treadle.executable.ExpressionViewRenderer

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
  * @param input              a firrtl program contained in a string
  * @param optionsManager     collection of options for the engine
  */
class TreadleTester(input: String, optionsManager: HasInterpreterSuite = new InterpreterOptionsManager) {
  var expectationsMet = 0

  treadle.random.setSeed(optionsManager.treadleOptions.randomSeed)

  val engine: ExecutionEngine                = ExecutionEngine(input, optionsManager)
  val interpreterOptions: TreadleOptions = optionsManager.treadleOptions

  setVerbose(interpreterOptions.setVerbose)

  if(interpreterOptions.writeVCD) {
    optionsManager.setTopNameIfNotSet(engine.ast.main)
    optionsManager.makeTargetDir()
    engine.makeVCDLogger(
      interpreterOptions.vcdOutputFileName(optionsManager),
      interpreterOptions.vcdShowUnderscored
    )
  }

  def setVerbose(value: Boolean = true): Unit = {
    engine.setVerbose(value)
  }

  val startTime: Long = System.nanoTime()

  def makeSnapshot(): Unit = {
    val snapshotName = optionsManager.getBuildFileName(".datastore.snapshot.json")
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
    if(engine.checkStopped(s"poke($name, $value)")) return

    try {
      val isRegister = engine.symbolTable.isRegister(name)
//      engine.circuitState.vcdLowerClock()
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
    if(engine.checkStopped(s"peek($name)")) return 0

    engine.getValue(name)
  }

  /**
    * require that a value be present on the named component
 *
    * @param name component name
    * @param expectedValue the BigInt value required
    */
  def expect(name: String, expectedValue: BigInt, message: String = ""): Unit = {
    engine.scheduler.executeInputSensitivities()
    val value = engine.getValue(name)
    if(value != expectedValue) {
      val renderer = new ExpressionViewRenderer(
        engine.dataStore, engine.symbolTable, engine.expressionViews)
      val calculation = renderer.render(engine.symbolTable(name))
      fail(new TreadleException (s"Error:expect($name, $expectedValue) got $value $message\n$calculation"))
    }
    expectationsMet += 1
  }

  var cycleCount: Long = 0L

  /**
    * Cycles the circuit n steps (with a default of one)
    * At each step registers and memories are advanced and all other elements recomputed
 *
    * @param n cycles to perform
    */
  def step(n: Int = 1): Unit = {
    if(engine.checkStopped(s"step($n)")) return

    for(_ <- 0 until n) {
      cycleCount += 1
      engine.cycle()
    }
  }

  /**
    * Pokes value to the named memory at offset
    *
    * @param name  the name of a memory
    * @param index the offset in the memory
    * @param value a value to put on that port
    */
  def pokeMemory(name: String, index: Int, value: BigInt): Unit = {
    if (engine.checkStopped(s"pokeMemory($name, $value)")) return

    engine.symbolTable.get(name) match {
      case Some(memory) =>
        engine.setValue(name, value = value, offset = index)
      case _ =>
        throw TreadleException(s"Error: memory $name.forceWrite($index, $value). memory not found")
    }
  }

  def peekMemory(name: String, index: Int): BigInt = {
    engine.symbolTable.get(name) match {
      case Some(memory) =>
        engine.getValue(name, offset = index)
      case _ =>
        throw TreadleException(s"Error: get memory $name.forceWrite($index). memory not found")
    }
  }


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
//      s"in ${engine.circuitState.stateCounter} cycles " +
      f"in $cycleCount cycles in $elapsedSeconds%.6f seconds"
  }
  /**
    * A simplistic report of the number of expects that passed and
    */
  def report(): Unit = {
    engine.writeVCD()
    println(reportString)
  }

  def finish: Boolean = {
    isOK
  }
}
