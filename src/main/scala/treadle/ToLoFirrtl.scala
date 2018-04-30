// See LICENSE for license details.

package treadle

import firrtl.passes.memlib.VerilogMemDelays
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.PrimOps.{Dshl, Shl}
import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object ToLoFirrtl extends Compiler {
  override def emitter: Emitter = new LowFirrtlEmitter
  override def transforms: Seq[Transform] = {
    getLoweringTransforms(ChirrtlForm, LowForm) ++
            Seq(new LowFirrtlOptimization, new BlackBoxSourceHelper, new FixupOps)
//    getLoweringTransforms(ChirrtlForm, LowForm) ++ Seq(new BlackBoxSourceHelper)
  }

  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlOptions with HasTreadleOptions): Circuit = {

    val annotations = optionsManager.firrtlOptions.annotations
    val compileResult = compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))

//    VerilogMemDelays.run(compileResult.circuit)
    compileResult.circuit
  }
}

// Workaround for https://github.com/freechipsproject/firrtl/issues/498
class FixupOps extends Transform {
  def inputForm  : CircuitForm = LowForm
  def outputForm : CircuitForm = HighForm

  private def onExpr(expr: Expression): Expression =
    expr.map(onExpr) match {
      case prim @ DoPrim(Dshlw,_,_,_) => prim.copy(op = Dshl)
      case prim @ DoPrim(Shlw,_,_,_) => prim.copy(op = Shl)
      case other => other
    }
  private def onStmt(stmt: Statement): Statement = stmt.map(onStmt).map(onExpr)
  private def onMod(mod: DefModule): DefModule = mod.map(onStmt)
  def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.map(onMod))
  }
}
