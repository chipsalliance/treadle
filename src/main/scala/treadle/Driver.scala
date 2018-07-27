// See LICENSE for license details.

package treadle

import firrtl.{AnnotationSeq, FirrtlCircuitAnnotation, FirrtlExecutionOptions, HasFirrtlExecutionOptions}
import firrtl.options.{DriverExecutionResult, ExecutionOptionsManager}
import firrtl.options.Viewer._
import firrtl.FirrtlViewer._

case class TreadleExecutionResult(treadleTesterOpt: Option[TreadleTester]) extends DriverExecutionResult

object Driver extends firrtl.options.Driver {

  def vcdOutputFileName(annotationSeq: AnnotationSeq): String = {
    val firrtlOptions = view[FirrtlExecutionOptions](annotationSeq).get
    s"${firrtlOptions.getBuildFileName("vcd")}"
  }

  def vcdInputFileName(annotationSeq: AnnotationSeq, fileNameOverride: Option[String] = None): String = {
    val firrtlOptions = view[FirrtlExecutionOptions](annotationSeq).get
    s"${firrtlOptions.getBuildFileName(suffix = "vcd", fileNameOverride )}"
  }

  val optionsManager: ExecutionOptionsManager = {
    new ExecutionOptionsManager("treadle") with HasFirrtlExecutionOptions
  }

  override def execute(args: Array[String], initialAnnotations: AnnotationSeq): DriverExecutionResult = {
    val annotations = optionsManager.parse(args, initialAnnotations)

    val firrtlInput = annotations.collectFirst {
      case circuit: FirrtlCircuitAnnotation => circuit.value.serialize

    }
    val tester = new TreadleTester(firrtlInput.get, initialAnnotations)
    TreadleExecutionResult(Some(tester))
  }
}

