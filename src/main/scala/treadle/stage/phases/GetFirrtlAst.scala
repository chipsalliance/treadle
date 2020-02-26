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

import firrtl.options.Phase
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlFileAnnotation, FirrtlSourceAnnotation}
import firrtl.{AnnotationSeq, Parser}

/**
  * There are multiple ways to get a FirrtlCircuit into treadle.
  * There is a priority to these methods
  * 1. Specify a Firrtl AST with the FirrtlCircuitAnnotation
  * 2. Specify Firrtl text with a FirrtlSourceAnnotation
  * 3. Specify a file containing Firrtl with the FirrtlFileAnnotation
  */
object GetFirrtlAst extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {

    /* first priority, does circuit already exist */
    def handleTreadleCircuit(): Option[AnnotationSeq] = {
      if (annotationSeq.exists { case FirrtlCircuitAnnotation(_) => true; case _ => false }) {
        Some(annotationSeq)
      } else {
        None
      }
    }

    /* second priority, does firrtl source exist */
    def handleFirrtlSource(): Option[AnnotationSeq] = {
      annotationSeq.collectFirst { case FirrtlSourceAnnotation(firrtlText) => firrtlText } match {
        case Some(text) =>
          val circuit = Parser.parse(text)
          Some(FirrtlCircuitAnnotation(circuit) +: annotationSeq)
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
          Some(FirrtlCircuitAnnotation(circuit) +: annotationSeq)
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
