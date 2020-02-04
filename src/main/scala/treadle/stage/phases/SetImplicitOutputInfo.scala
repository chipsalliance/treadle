// See LICENSE for license details.

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
    val nameAnnoSeq = if(annotationSeq.exists { case _: TargetDirAnnotation => true ; case _ => false }) {
      Seq.empty
    }
    else {
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
      Seq(OutputFileAnnotation(name))
    }
    val targetDir = if(annotationSeq.exists { case _: TargetDirAnnotation => true ; case _ => false }) {
      Seq.empty
    }
    else {
      Seq(TargetDirAnnotation(s"test_run_dir/$outputFileName"))
    }
    annotationSeq ++ nameAnnoSeq ++ targetDir
  }
}
