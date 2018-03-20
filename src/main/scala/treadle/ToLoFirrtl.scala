// See LICENSE for license details.

package treadle

import firrtl._
import firrtl.ir.Circuit

object ToLoFirrtl {
  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlOptions with HasTreadleOptions): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotations = optionsManager.firrtlOptions.annotations
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))
    compileResult.circuit
  }
}
