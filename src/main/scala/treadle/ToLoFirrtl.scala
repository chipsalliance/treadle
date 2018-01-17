// See LICENSE for license details.

package treadle

import firrtl._
import firrtl.ir.Circuit

object ToLoFirrtl {
  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlOptions with HasInterpreterOptions): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotationMap = AnnotationMap(optionsManager.firrtlOptions.annotations)
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, Some(annotationMap)))
    compileResult.circuit
  }
}
