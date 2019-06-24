// See LICENSE for license details.

package treadle.stage.phases

import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.options.Phase
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.{AnnotationSeq, ChirrtlForm, CircuitState, HighForm, LowFirrtlOptimization, LowForm, Transform}
import treadle.TreadleCircuitStateAnnotation
import treadle.utils.{AugmentPrintf, FixupOps}

trait TreadlePhase extends Phase {
  val transforms: Seq[Transform]

  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {
    annotationSeq.flatMap {
      case FirrtlCircuitAnnotation(circuit) =>
        val state = CircuitState(circuit, HighForm, annotationSeq)
        val newState = transforms.foldLeft(state) { case (prevState, transform) => transform.runTransform(prevState) }
        Some(TreadleCircuitStateAnnotation(newState))
      case other =>
        Some(other)
    }
  }
}

/** Prepare the AST from low FIRRTL.
  *
  */
object PrepareAstFromLowFIRRTL extends TreadlePhase {
  val transforms: Seq[Transform] = Seq(new FixupOps, AugmentPrintf)
}
/**
  * Call a bunch of transforms so TreadleTester can operate
  */
object PrepareAst extends TreadlePhase {
  val transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, LowForm) ++
    Seq(new LowFirrtlOptimization, new BlackBoxSourceHelper, new FixupOps, AugmentPrintf)
}
