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

package treadle.utils

import firrtl.Mappers._
import firrtl.PrimOps.Dshl
import firrtl._
import firrtl.ir._
import firrtl.stage.Forms

/**
  *  Workaround for https://github.com/freechipsproject/firrtl/issues/498 from @jackkoenig
  */
class FixupOps extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.LowForm
  override def optionalPrerequisites = Forms.LowFormOptimized
  override def optionalPrerequisiteOf = Forms.LowEmitters
  override def invalidates(a: Transform) = false

  private def onExpr(expr: Expression): Expression =
    expr.map(onExpr) match {
      case prim @ DoPrim(Dshlw, _, _, _) => prim.copy(op = Dshl)
      case other                         => other
    }

  private def onStmt(stmt: Statement): Statement = stmt.map(onStmt).map(onExpr)
  private def onMod(mod:   DefModule): DefModule = mod.map(onStmt)
  def execute(state:       CircuitState): CircuitState = {
    state.copy(circuit = state.circuit.map(onMod))
  }
}
