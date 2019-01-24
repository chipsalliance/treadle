// See LICENSE for license details.

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.FirrtlSourceAnnotation
import treadle.TreadleTester
import treadle.stage.TreadleTesterAnnotation

object ConstructTester extends Phase {
  def transform(a: AnnotationSeq): AnnotationSeq = {
    a.flatMap {
      case FirrtlSourceAnnotation(firrtlSource) =>
        Seq(TreadleTesterAnnotation(new TreadleTester(firrtlSource)))
      case other => Seq(other)
    }
  }
}
