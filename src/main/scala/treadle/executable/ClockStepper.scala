// See LICENSE for license details.

package treadle.executable

import treadle.{ExecutionEngine, TreadleException}
import treadle.chronometry.UTC

import scala.collection.mutable

trait ClockStepper {
  var cycleCount: Long = 0L
  def run(steps: Int): Unit
  def getCycleCount: Long = cycleCount
  def addTask(taskTime: Long)(task: () => Unit): Unit
  val clockAssigners: mutable.HashMap[Symbol, ClockAssigners] = new mutable.HashMap()
  def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {}
}

class NoClockStepper extends ClockStepper {
  override def run(steps: Int): Unit = {}

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    throw TreadleException(s"Timed task cannot be added to circuits with no clock")
  }

  val clockSymbols: Set[Symbol] = Set.empty
}

case class ClockAssigners(upAssigner: Assigner, downAssigner: Assigner)

case class SimpleSingleClockStepper(
  engine: ExecutionEngine,
  dataStore: DataStore,
  clockSymbol: Symbol,
  resetSymbolOpt: Option[Symbol],
  clockPeriod: Long,
  clockInitialOffset: Long,
  wallTime: UTC
) extends ClockStepper {

  var clockIsHigh: Boolean = false
  def clockIsLow: Boolean = ! clockIsHigh

  val upPeriod   : Long = clockPeriod / 2
  val downPeriod : Long = clockPeriod - upPeriod

  var resetTaskTime: Long = -1L

  val hasRollBack: Boolean = engine.dataStore.numberOfBuffers > 0

  val clockAssigner = dataStore.TriggerConstantAssigner(clockSymbol, engine.scheduler, triggerOnValue = 1)
  engine.scheduler.clockAssigners += clockAssigner
  engine.scheduler.addAssigner(clockSymbol, clockAssigner, excludeFromCombinational = true)

  clockAssigners(clockSymbol) = ClockAssigners(clockAssigner, clockAssigner)

  /**
    * This function is (and should only) be used by the VcdReplayTester
    * @param clockSymbol clock to bump
    * @param value        new clock value should be zero or one, all non-zero values are treated as one
    */
  override def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {
    if(hasRollBack) {
      // save data state under roll back buffers for this clock
      engine.dataStore.saveData(clockSymbol.name, wallTime.currentTime)
    }

    val constantAssigner = clockAssigner
    constantAssigner.value = if(value > Big(0)) {
      if(hasRollBack) {
        // save data state under roll back buffers for this clock
        engine.dataStore.saveData(clockSymbol.name, wallTime.currentTime)
      }
      1
    }
    else {
      0
    }
    constantAssigner.run()
  }

  /**
    * Execute specified number of clock cycles (steps)
    * @param steps number of clock cycles to advance
    */
  override def run(steps: Int): Unit = {
    for(_ <- 0 until steps) {
      if(engine.verbose) {
        println(s"step: ${cycleCount + 1} started")
      }
      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      cycleCount += 1

      /* This bit of code assumes any combinational delays occur after a down clock has happened. */
      val downIncrement = (wallTime.currentTime - clockInitialOffset) % clockPeriod
      wallTime.incrementTime(downPeriod - downIncrement)
      if(resetTaskTime >= 0 && wallTime.currentTime >= resetTaskTime) {
        resetSymbolOpt.foreach { resetSymbol =>
          engine.setValue(resetSymbol.name, 0)
          engine.inputsChanged = true
          engine.evaluateCircuit()
        }
        resetTaskTime = -1L
      }

      if(hasRollBack) {
        // save data state under roll back buffers for this clock
        engine.dataStore.saveData(clockSymbol.name, wallTime.currentTime)
      }

      clockAssigner.value = 1
      clockAssigner.run()
      engine.inputsChanged = true
      engine.evaluateCircuit()
      wallTime.incrementTime(upPeriod)

      if(resetTaskTime >= 0 && wallTime.currentTime >= resetTaskTime) {
        resetSymbolOpt.foreach { resetSymbol =>
          engine.setValue(resetSymbol.name, 0)
          engine.inputsChanged = true
          engine.evaluateCircuit()
        }
        resetTaskTime = -1L
      }
      clockAssigner.value = 0
      clockAssigner.run()
      if(engine.verbose) {
        println(s"step: $cycleCount finished")
      }
    }
  }

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    if(resetTaskTime >= 0) {
      throw TreadleException(s"Timed add second reset task to single clock")
    }
    resetTaskTime = taskTime
  }
}

class MultiClockStepper(engine: ExecutionEngine, clockInfoList: Seq[ClockInfo], wallTime: UTC) extends ClockStepper {
  val dataStore: DataStore = engine.dataStore
  val scheduler: Scheduler = engine.scheduler
  val hasRollBack: Boolean = engine.dataStore.numberOfBuffers > 0

  val shortestPeriod: Long = clockInfoList.map(_.period).min

  clockInfoList.foreach { clockInfo =>
    val clockSymbol = engine.symbolTable(clockInfo.name)

    val clockUpAssigner = dataStore.TriggerExpressionAssigner(
      clockSymbol, scheduler, GetIntConstant(1).apply, triggerOnValue = 1)

    val clockDownAssigner = dataStore.TriggerExpressionAssigner(
      clockSymbol, scheduler, GetIntConstant(0).apply, triggerOnValue = -1)

    clockAssigners(clockSymbol) = ClockAssigners(clockUpAssigner, clockDownAssigner)


    scheduler.clockAssigners += clockUpAssigner
    scheduler.clockAssigners += clockDownAssigner

    // this sets clock high and will call register updates
    wallTime.addRecurringTask(clockInfo.period, clockInfo.initialOffset, s"${clockInfo.name}/up") { () =>
      if(hasRollBack) {
        // save data state under roll back buffers for this clock
        engine.dataStore.saveData(clockInfo.name, wallTime.currentTime)
      }
      clockUpAssigner.run()
      engine.inputsChanged = true
    }

    // this task sets clocks low
    wallTime.addRecurringTask(
      clockInfo.period,
      clockInfo.initialOffset + clockInfo.upPeriod,
      s"${clockInfo.name}/down"
    ) { () =>
      clockDownAssigner.run()
    }
  }

  /**
    * This function is (and should only) be used by the VcdReplayTester
    * @param clockSymbol clock to bump
    * @param value        new clock value should be zero or one, all non-zero values are treated as one
    */
  override def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {
    val assigner = clockAssigners(clockSymbol)
    if(value > Big(0)) {
      if(hasRollBack) {
        // save data state under roll back buffers for this clock
        engine.dataStore.saveData(clockSymbol.name, wallTime.currentTime)
      }
      assigner.upAssigner.run()
    }
    else {
      assigner.downAssigner.run()
    }
  }

  /**
    * One step is defined here as the running until the next up clock transition
    * @param steps the number of up clocks to find and execute
    */
  override def run(steps: Int): Unit = {

    for (_ <- 0 until steps) {
      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      var upTransitionProcessed = false

      def runHeadTask(): Unit = {
        wallTime.runNextTask().foreach { taskRun =>
          if (taskRun.taskName.endsWith("/up")) {
            cycleCount += 1
            upTransitionProcessed = true
          }
        }
      }

      while(! upTransitionProcessed) {
        runHeadTask()
      }

      /*
      there could be multiple clocks temporarily set to run at this
      same time, let them all run
       */
      while(wallTime.eventQueue.head.time == wallTime.currentTime) {
        runHeadTask()
      }
    }
  }

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    wallTime.addOneTimeTask(taskTime)(task)
  }
}
