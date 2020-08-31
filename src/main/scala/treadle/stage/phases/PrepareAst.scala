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

import firrtl.options.{Dependency, Phase}
import firrtl.stage.{FirrtlCircuitAnnotation, Forms}
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.{AnnotationSeq, CircuitState, passes}
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
