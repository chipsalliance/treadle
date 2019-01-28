// See LICENSE for license details.

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.FirrtlSourceAnnotation
import treadle.TreadleTester
import treadle.stage.TreadleTesterAnnotation

object ConstructTester extends Phase {
  /**
    * Makes a TreadleTesters
    * Wraps it in an Annotation
    * Adds the annotation to the annotation Seq
    * Returns the updated Seq
    * @param a
    * @return
    */
  def transform(a: AnnotationSeq): AnnotationSeq = {
    a.flatMap {
      case FirrtlSourceAnnotation(_) =>
        Seq(TreadleTesterAnnotation(TreadleTester(a)))
      case other => Seq(other)
    }
  }
}
