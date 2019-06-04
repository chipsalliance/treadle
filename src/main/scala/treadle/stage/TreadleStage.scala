// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, Shell, Stage}
import firrtl.stage.FirrtlCli
import treadle.stage.phases.{CreateTester, GetFirrtlAst, PrepareAst, SetImplicitOutputInfo}

class TreadleCompatibilityStage extends Stage {
  override val shell: Shell = new Shell("treadle") with FirrtlCli

  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAst
  )

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    phases.foldLeft(annotations)( (a, f) => f.transform(a) )
  }
}

class TreadleStage extends Stage {
  override val shell: Shell = new Shell("treadle") with FirrtlCli

  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    PrepareAst,
    CreateTester
  )

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    phases.foldLeft(annotations)( (a, f) => f.transform(a) )
  }
}
