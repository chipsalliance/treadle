// See LICENSE for license details.

package treadle.utils

import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.Mappers._
import firrtl.PrimOps.Dshl
import firrtl._
import firrtl.ir._
import firrtl.transforms.BlackBoxSourceHelper
import treadle.HasTreadleOptions

/**
  * Use these lowering transforms to prepare circuit for compiling
  */
object ToLoFirrtl extends Compiler {
  override def emitter: Emitter = new LowFirrtlEmitter
  override def transforms: Seq[Transform] = {
    getLoweringTransforms(ChirrtlForm, LowForm) ++
            Seq(new LowFirrtlOptimization, new BlackBoxSourceHelper, new FixupOps)
  }

  def lower(c: Circuit,
            optionsManager: ExecutionOptionsManager with HasFirrtlOptions with HasTreadleOptions): Circuit = {

    val annotations = optionsManager.firrtlOptions.annotations
    val compileResult = compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))

    compileResult.circuit
  }
}

/**
  *  Workaround for https://github.com/freechipsproject/firrtl/issues/498 from @jackkoenig
  */
class FixupOps extends Transform {
  def inputForm  : CircuitForm = LowForm
  def outputForm : CircuitForm = HighForm

  private def onExpr(expr: Expression): Expression =
    expr.map(onExpr) match {
      case prim @ DoPrim(Dshlw,_,_,_) => prim.copy(op = Dshl)
      case other => other
    }
  private def onStmt(stmt: Statement): Statement = stmt.map(onStmt).map(onExpr)
  private def onMod(mod: DefModule): DefModule = mod.map(onStmt)
  def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.map(onMod))
  }
}
