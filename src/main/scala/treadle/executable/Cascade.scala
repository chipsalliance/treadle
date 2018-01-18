// See LICENSE for license details.

package treadle.executable

class Cascade(
    val symbol: Symbol,
    dataStore: DataStore,
    transition: Transition,
    assigner: Assigner,
    scheduler: Scheduler,
    immediateAssigns: Seq[Assigner],
    sensitiveAssigns: Seq[Assigner]
) extends Assigner {

  def runImmediateAssigns(): Unit = {
    scheduler.executeAssigners(immediateAssigns)
  }

  def testAndRun: Unit = {
    val previousValue = dataStore.currentIntArray(symbol.index)
    assigner.run
    val afterValue = dataStore.currentIntArray(symbol.index)
    (transition, previousValue, afterValue) match {
      case (PositiveEdge, 0, 1) =>
        runImmediateAssigns()
      case (NegativeEdge, 1, 0) =>
        runImmediateAssigns()
      case (Changed, before, after) if before != after =>
        runImmediateAssigns()
    }
  }

  override def run: FuncUnit = testAndRun _
}

trait Transition

case object PositiveEdge extends Transition

case object NegativeEdge extends Transition

case object Changed extends Transition


