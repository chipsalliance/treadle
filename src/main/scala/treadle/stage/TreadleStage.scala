/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
    chirrtlPhases.foldLeft(annotations)((a, f) => f.transform(a))
  }

  /** Here to determine which phases to run based on the circuit form.
    *
    */
  def checkFormTransform(circuitForm: CircuitForm, annotations: AnnotationSeq): AnnotationSeq = {
    val phases = circuitForm match {
      case ChirrtlForm => chirrtlPhases
      case _           => lowFIRRTLPhases
    }
    phases.foldLeft(annotations)((a, f) => f.transform(a))
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
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  }
}
