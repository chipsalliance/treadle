// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, Shell, Stage}
import firrtl.stage.FirrtlCli
import treadle.stage.phases.{CreateTester, GetFirrtlAst, PrepareAst, SetImplicitOutputInfo}

object TreadleCompatibilityPhase extends Phase {
  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAst
  )

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
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
