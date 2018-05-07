// See LICENSE for license details.

package treadle.executable

import treadle.{ExecutionEngine, TreadleException}
import treadle.chronometry.UTC

trait ClockStepper {
  var cycleCount: Long = 0L
  def run(steps: Int): Unit
  def getCycleCount: Long = cycleCount
  def addTask(taskTime: Long)(task: () => Unit): Unit
}

class NoClockStepper extends ClockStepper {
  override def run(steps: Int): Unit = {}

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    throw TreadleException(s"Timed task cannot be added to circuits with no clock")
  }
}

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

  val clockAssigner = dataStore.TriggerConstantAssigner(clockSymbol, engine.scheduler, triggerOnValue = 1)
  engine.scheduler.clockAssigners += clockAssigner
  engine.scheduler.addAssigner(clockSymbol, clockAssigner, excludeFromCombinational = true)

  override def run(steps: Int): Unit = {

    for(_ <- 0 until steps) {
      if(engine.verbose) {
        println(s"step: ${cycleCount + 1} started")
      }
      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      cycleCount += 1

      /*
      This bit of code assumes any combinational delays occur after a down clock has happened.
       */
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

  clockInfoList.foreach { clockInfo =>
    val clockSymbol = engine.symbolTable(clockInfo.name)

    val clockUpAssigner = dataStore.TriggerExpressionAssigner(
      clockSymbol, scheduler, GetIntConstant(1).apply, triggerOnValue = 1)

    val clockDownAssigner = dataStore.TriggerExpressionAssigner(
      clockSymbol, scheduler, GetIntConstant(0).apply, triggerOnValue = -1)

    scheduler.clockAssigners += clockUpAssigner
    scheduler.clockAssigners += clockDownAssigner

    // this sets clock high and will call register updates
    wallTime.addRecurringTask(clockInfo.period, clockInfo.initialOffset, s"${clockInfo.name}/up") { () =>
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
            val clockName = taskRun.taskName.split("/").head
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
