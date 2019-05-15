// See LICENSE for license details.

package treadle.stage.phases

import firrtl.annotations.Annotation
import firrtl.{AnnotationSeq, Parser}
import firrtl.options.Phase
import firrtl.stage.FirrtlSourceAnnotation
import treadle.TreadleCircuitAnnotation
import treadle.executable.TreadleException

import scala.collection.mutable

object GetFirrtlAst extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {
    var sourcesFound = new mutable.ArrayBuffer[Annotation]()

    val newAnnotationSeq = annotationSeq.flatMap {
      case fs @ FirrtlSourceAnnotation(firrtlText) =>
        sourcesFound += fs
        val circuit = Parser.parse(firrtlText)
        Some(TreadleCircuitAnnotation(circuit))

      case other => Some(other)
    }

    if(sourcesFound.length > 1) {
      throw TreadleException(s"Multiple sources of firrtl found for Treadle" + sourcesFound.mkString("\n", "\n", ""))
    }

    newAnnotationSeq
  }
}
