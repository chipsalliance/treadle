// See LICENSE for license details.

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.Phase
import treadle.{TreadleCircuitAnnotation, TreadleCircuitStateAnnotation, TreadleTester, TreadleTesterAnnotation}

object CreateTester extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    if(a.exists {
      case TreadleCircuitAnnotation(_) => true
      case TreadleCircuitStateAnnotation(_) => true
      case _ => false
    }
    ) {
      val tester = new TreadleTester(a)
      a :+ TreadleTesterAnnotation(tester)
    }
    else {
      a
    }
  }
}
