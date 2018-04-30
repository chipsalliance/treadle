// See LICENSE for license details.

package treadle

import firrtl.{ExecutionOptionsManager, HasFirrtlExecutionOptions, LowFirrtlCompiler, CircuitState, ChirrtlForm}
import firrtl.ir.Circuit

object ToLoFirrtl {
  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions with HasTreadleOptions): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotations = optionsManager.firrtlOptions.annotations
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))
    compileResult.circuit
  }
}
