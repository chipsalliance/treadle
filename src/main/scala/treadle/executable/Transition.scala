// See LICENSE for license details.

package treadle.executable

trait Transition

//TODO: (chick) incorporate NegativeEdge usage.

case object PositiveEdge extends Transition
case object NegativeEdge extends Transition
case object NoTransition extends Transition
