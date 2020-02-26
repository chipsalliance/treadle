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

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.{Phase, TargetDirAnnotation}
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation
import firrtl.stage.{FirrtlCircuitAnnotation, OutputFileAnnotation}
import treadle.TreadleCircuitStateAnnotation

/**
  * Set a default output stuff
  * Sets the default target directory if one has not been defined
  * and uses the circuit name unless there is as TopName override
  */
object SetImplicitOutputInfo extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {
    var outputFileName = "default"
    val nameAnnoSeq = if (annotationSeq.exists { case _: TargetDirAnnotation => true; case _ => false }) {
      Seq.empty
    } else {
      outputFileName = annotationSeq.collectFirst {
        case TopNameAnnotation(topName) =>
          topName
      }.getOrElse {
        annotationSeq.collectFirst {
          case FirrtlCircuitAnnotation(circuit) =>
            circuit.main
          case TreadleCircuitStateAnnotation(state) =>
            state.circuit.main
        }.getOrElse("default")
      }
      Seq(OutputFileAnnotation(outputFileName))
    }
    val targetDir = if (annotationSeq.exists { case _: TargetDirAnnotation => true; case _ => false }) {
      Seq.empty
    } else {
      Seq(TargetDirAnnotation(s"test_run_dir/$outputFileName"))
    }
    annotationSeq ++ nameAnnoSeq ++ targetDir
  }
}
