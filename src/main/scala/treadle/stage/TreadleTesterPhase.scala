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

import firrtl.AnnotationSeq
import firrtl.options.Phase
import treadle.stage.phases._

/**
  * When returns the annotation list with a TreadleTester constructed
  * from either a circuit, a file, or a string
  */
class TreadleTesterPhase extends Phase {
  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    new PrepareAst,
    CreateTester
  )

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  }
}
