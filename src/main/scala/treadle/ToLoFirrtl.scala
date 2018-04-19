// See LICENSE for license details.

package treadle

import firrtl._
import firrtl.ir.Circuit
import firrtl.passes.memlib.VerilogMemDelays

object ToLoFirrtl {
  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlOptions with HasTreadleOptions): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotations = optionsManager.firrtlOptions.annotations
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))

//    VerilogMemDelays.run(compileResult.circuit)
    compileResult.circuit
  }
}
