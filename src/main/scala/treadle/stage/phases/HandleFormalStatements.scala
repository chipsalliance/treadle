// SPDX-License-Identifier: Apache-2.0

package treadle.stage.phases

import firrtl.PrimOps.{And, Not}
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Dependency, RegisteredTransform, ShellOption}
import firrtl.passes.ExpandWhensAndCheck
import firrtl.stage.TransformManager.TransformDependency
import firrtl.{CircuitState, DependencyAPIMigration, Transform}

/** This controls the handling of the verification formal statements for treadle.
  * currently it does the following
  * converts assert statements to a printf / stop block
  * by default it will also do this for assumne statements
  * but assume statement can be dropped by using "tr-ignore-format-assumes" or [IgnoreFormalAssumesAnnotation]
  * cover statement are currently skipped
  *
  */
class HandleFormalStatements
  extends Transform
    with RegisteredTransform
    with DependencyAPIMigration {

  override def prerequisites: Seq[TransformDependency] = Seq(Dependency[ExpandWhensAndCheck])

  override def optionalPrerequisites: Seq[TransformDependency] = Seq.empty

  override def optionalPrerequisiteOf: Seq[TransformDependency] =
    firrtl.stage.Forms.MidEmitters

  override def invalidates(a: Transform): Boolean = false

  val options = Seq(
    new ShellOption[Unit](
      longOption = "tr-ignore-formal-assumes",
      toAnnotationSeq = (_: Unit) => Seq(IgnoreFormalAssumesAnnotation),
      helpText = "Will ignore Forma Assume statements"
    )
  )

  def run(c: Circuit, dropAssumes: Boolean): Circuit = {
    def assertAssumption(s: Statement): Statement = {
      def makeTrigger(cond: Expression, en: Expression): Expression = {
        val notOfCondition = DoPrim(Not, Seq(cond), Nil, cond.tpe)
        DoPrim(And, Seq(notOfCondition, en), Nil, cond.tpe)
      }

      def makeBlock(info: Info, clk: Expression, trigger: Expression, msg: StringLit, stopValue: Int): Statement = {
        val stop = Stop(info, ret = stopValue, clk, trigger)
        msg match {
          case StringLit("") => stop
          case _ =>
            Block(
              Print(info, msg, Seq.empty, clk, trigger),
              stop
            )
        }
      }

      s match {
        case Verification(Formal.Assume, info, clk, cond, en, msg) =>
          if (dropAssumes) {
            EmptyStmt
          } else {
            makeBlock(info, clk, makeTrigger(cond, en), msg, 0x42)
          }

        case Verification(Formal.Assert, info, clk, cond, en, msg) =>
          makeBlock(info, clk, makeTrigger(cond, en), msg, 0x41)

        case t => t.mapStmt(assertAssumption)
      }
    }

    c.mapModule(mod => {
      mod.mapStmt(assertAssumption)
    })
  }

  def execute(state: CircuitState): CircuitState = {
    val dropAssumes = state.annotations.contains(IgnoreFormalAssumesAnnotation)
    state.copy(circuit = run(state.circuit, dropAssumes))
  }
}

case object IgnoreFormalAssumesAnnotation extends NoTargetAnnotation {
  val transform = new HandleFormalStatements
}

case object DontAssertAllAssumptionsAnnotation extends NoTargetAnnotation
