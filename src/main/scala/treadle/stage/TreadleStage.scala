// See LICENSE for license details.

package treadle.stage

import firrtl.{AnnotationSeq, ChirrtlForm, CircuitForm, LowForm}
import firrtl.options.{Phase, Shell, Stage}
import firrtl.stage.FirrtlCli
import treadle.stage.phases.{CreateTester, GetFirrtlAst, PrepareAst, PrepareAstFromLowFIRRTL, SetImplicitOutputInfo}

object TreadleCompatibilityPhase extends Phase {
  private val chirrtlPhases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAst
  )
  private val lowFIRRTLPhases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAstFromLowFIRRTL
  )

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    chirrtlPhases.foldLeft(annotations)( (a, f) => f.transform(a) )
  }

  /** Here to determine which phases to run based on the circuit form.
    *
    */
  def checkFormTransform(circuitForm: CircuitForm, annotations: AnnotationSeq): AnnotationSeq = {
    val phases = circuitForm match {
      case ChirrtlForm => chirrtlPhases
      case LowForm => lowFIRRTLPhases
    }
    phases.foldLeft(annotations)( (a, f) => f.transform(a) )
  }
}

/**
  * When returns the annotation list with a TreadleTester constructed
  * from either a circuit, a file, or a string
  */
object TreadleTesterPhase extends Phase {
  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAst,
    CreateTester
  )

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    phases.foldLeft(annotations)( (a, f) => f.transform(a) )
  }
}
