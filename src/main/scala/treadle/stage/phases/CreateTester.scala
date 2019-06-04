// See LICENSE for license details.

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.Phase
import treadle.{TreadleTester, TreadleTesterAnnotation}

object CreateTester extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    val tester = new TreadleTester(a)
    a :+ TreadleTesterAnnotation(tester)
  }
}
