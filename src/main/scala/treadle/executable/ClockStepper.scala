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

class MultiClockStepper(engine: ExecutionEngine, clockName: String, wallTime: UTC) extends ClockStepper {
  override def run(steps: Int): Unit = {
    for (_ <- 0 until steps) {
      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      cycleCount += 1
      if (engine.verbose) println(s"step $cycleCount at ${wallTime.currentTime}")
      wallTime.runToTask(s"$clockName/up")
      wallTime.runUntil(wallTime.currentTime)
      if (engine.verbose) println(s"clock raised at ${wallTime.currentTime}")
      engine.evaluateCircuit()
      wallTime.runToTask(s"$clockName/down")
      wallTime.runUntil(wallTime.currentTime)
      if (engine.verbose) println(s"Step finished step at ${wallTime.currentTime}")
    }
  }

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    wallTime.addOneTimeTask(taskTime)(task)
  }
}
