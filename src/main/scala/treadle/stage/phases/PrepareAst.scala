// SPDX-License-Identifier: Apache-2.0

package treadle.stage.phases

import firrtl.options.{Dependency, Phase}
import firrtl.stage.{FirrtlCircuitAnnotation, Forms}
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.{passes, AnnotationSeq, CircuitState}
import treadle.TreadleCircuitStateAnnotation
import treadle.utils.{AugmentPrintf, FixupOps}

/**
  * Call a bunch of transforms so TreadleTester can operate
  */
class PrepareAst extends Phase {
  private val targets = Seq(
    Dependency[BlackBoxSourceHelper],
    Dependency[FixupOps],
    Dependency[AugmentPrintf]
  ) ++ Forms.LowForm ++ Seq(
    Dependency(passes.RemoveValidIf),
    Dependency(passes.memlib.VerilogMemDelays),
    Dependency(passes.SplitExpressions),
    Dependency[firrtl.transforms.LegalizeAndReductionsTransform],
    Dependency[firrtl.transforms.ConstantPropagation],
    Dependency[firrtl.transforms.CombineCats],
    Dependency(passes.CommonSubexpressionElimination),
    Dependency[firrtl.transforms.DeadCodeElimination]
  )

  private def compiler = new firrtl.stage.transforms.Compiler(targets, currentState = Nil)
  private val transforms = compiler.flattenedTransformOrder

  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {

    annotationSeq.flatMap {
      case FirrtlCircuitAnnotation(circuit) =>
        val state = CircuitState(circuit, annotationSeq)
        val newState = transforms.foldLeft(state) {
          case (prevState, transform) => transform.runTransform(prevState)
        }
        Some(TreadleCircuitStateAnnotation(newState))
      case other =>
        Some(other)
    }
  }
}
