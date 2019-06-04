// See LICENSE for license details.

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.{Phase, TargetDirAnnotation}
import firrtl.stage.OutputFileAnnotation
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation
import treadle.{TreadleCircuitStateAnnotation, TreadleCircuitAnnotation}

/**
  * Set a default output stuff
  * Sets the default target directory if one has not been defined
  * and uses the circuit name unless there is as TopName override
  */
object SetImplicitOutputInfo extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {
    val name = annotationSeq.collectFirst {
      case TopNameAnnotation(topName) =>
        topName
    }.getOrElse {
      annotationSeq.collectFirst {
        case TreadleCircuitAnnotation(circuit) =>
          circuit.main
        case TreadleCircuitStateAnnotation(state) =>
          state.circuit.main
      }.getOrElse("a")
    }
    val targetDir = if(annotationSeq.exists { case t: TargetDirAnnotation => true ; case _ => false }) {
      Seq.empty
    }
    else {
      Seq(TargetDirAnnotation(s"test_run_dir/$name"))
    }
    (OutputFileAnnotation(name) +: annotationSeq) ++ targetDir
  }
}
