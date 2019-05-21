// See LICENSE for license details.

package treadle.stage.phases

import firrtl.annotations.Annotation
import firrtl.{AnnotationSeq, Parser}
import firrtl.options.Phase
import firrtl.stage.{FirrtlFileAnnotation, FirrtlSourceAnnotation}
import treadle.TreadleCircuitAnnotation
import treadle.executable.TreadleException

import scala.collection.mutable

/**
  * There are multiple ways to get a FirrtlCircuit into treadle.
  * There is a priority to these methods
  * 1. Specify a Firrtl AST with the TreadleCircuitAnnotation
  * 2. Specify Firrtl text with a FirrtlSourceAnnotation
  * 3. Specify a file containing Firrtl with the FirrtlFileAnnotation
  */
object GetFirrtlAst extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {

    /* first priority, does circuit already exist */
    def handleTreadleCircuit(): Option[AnnotationSeq] = {
      if(annotationSeq.exists { case TreadleCircuitAnnotation(_) => true ; case _ => false}) {
        Some(annotationSeq)
      }
      else {
        None
      }
    }

    /* second priority, does firrtl source exist */
    def handleFirrtlSource(): Option[AnnotationSeq] = {
      annotationSeq.collectFirst { case FirrtlSourceAnnotation(firrtlText) => firrtlText } match {
        case Some(text) =>
          val circuit = Parser.parse(text)
          Some(TreadleCircuitAnnotation(circuit) +: annotationSeq)
        case _ =>
          None
      }
    }

    /* third priority, does firrtl file exist */
    def handleFirrtlFile(): Option[AnnotationSeq] = {
      annotationSeq.collectFirst { case FirrtlFileAnnotation(fileName) => fileName } match {
        case Some(fileName) =>
          val file = io.Source.fromFile(fileName)
          val text = file.mkString
          file.close()
          val circuit = Parser.parse(text)
          Some(TreadleCircuitAnnotation(circuit) +: annotationSeq)
        case _ =>
          None
      }
    }

    val newAnnotations = handleTreadleCircuit().getOrElse {
      handleFirrtlSource().getOrElse {
        handleFirrtlFile().getOrElse {
          annotationSeq
        }
      }
    }
    newAnnotations
  }
}
