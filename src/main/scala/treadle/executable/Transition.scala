// See LICENSE for license details.

package treadle.executable
import firrtl.ir.Info

trait Transition

case object PositiveEdge extends Transition
case object NegativeEdge extends Transition
case object NoTransition extends Transition

trait AbstractClockTransitionGetter {
  def isPosEdge: Boolean
  def isNegEdge: Boolean
  def getClockSymbol : Symbol

  def transition: Transition = {
    if(isPosEdge) { PositiveEdge }
    else if(isNegEdge) { NegativeEdge }
    else { NoTransition }
  }
}

case class ClockBasedAssigner(
  assigner: Assigner,
  private val clockTransitionGetter : AbstractClockTransitionGetter,
  requiredTransition: Transition
) extends Assigner {

  override val symbol: Symbol = assigner.symbol
  override val info: Info = assigner.info
  private val clockSymbol = clockTransitionGetter.getClockSymbol

  def runLean(): Unit = {
    if(clockTransitionGetter.transition == requiredTransition) {
      assigner.run()
    }
  }

  def runFull(): Unit = {
    if(clockTransitionGetter.transition == requiredTransition) {
      assigner.run()
    }
    else if(isVerbose) {
      println(
        s"${assigner.symbol.name} <= register not updated" +
            s" ${clockSymbol.name} state ${clockTransitionGetter.transition} not the required $requiredTransition")
    }
  }

  var run: FuncUnit = runLean _

  override def setLeanMode(isLean: Boolean): Unit = {
    assigner.setLeanMode(isLean)
    run = if(isLean) runLean _ else runFull _
  }
}
