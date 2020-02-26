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

import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.ir.Circuit

import scala.collection.mutable

/**
  * Printf statements that print registers will show wrong values
  * unless this pass adds a delay for each register
  */
object AugmentPrintf extends Transform {
  def apply(circuit: Circuit): Circuit = {
    import firrtl.ir._
    import firrtl._
    def insert(newStmts: mutable.ArrayBuffer[Statement], namespace: Namespace, info: Info, clockExpression: Expression)(
      a:                 Expression
    ): Expression = {
      val newName = namespace.newTemp
      val wref = WRef(newName, a.tpe, NodeKind, SourceFlow)
      newStmts += DefRegister(info, newName, a.tpe, clockExpression, UIntLiteral(0), UIntLiteral(0))
      newStmts += Connect(info, wref, a)
      wref
    }

    def fixPrintsStmt(namespace: firrtl.Namespace)(s: Statement): Statement =
      s.mapStmt(fixPrintsStmt(namespace)) match {
        case s: Stop =>
          val newStmts = mutable.ArrayBuffer[Statement]()
          val newStop = s.mapExpr(insert(newStmts, namespace, s.info, s.clk))
          Block(newStmts :+ newStop)
        case p: Print =>
          val newStmts = mutable.ArrayBuffer[Statement]()
          val newName = namespace.newTemp
          val wref = WRef(newName, p.en.tpe, NodeKind, SourceFlow)
          newStmts += DefRegister(p.info, newName, p.en.tpe, p.clk, UIntLiteral(0), UIntLiteral(0))
          newStmts += Connect(p.info, wref, p.en)

          val newPrint: Print =
            p.mapExpr(insert(newStmts, namespace, p.info, p.clk)).asInstanceOf[Print].copy(en = wref)
          Block(newStmts :+ newPrint)
        case other => other
      }

    circuit.mapModule { m =>
      val ns = Namespace(m)
      m.mapStmt(fixPrintsStmt(ns))
    }
  }

  override def inputForm: CircuitForm = LowForm

  override def outputForm: CircuitForm = LowForm

  override protected def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = apply(state.circuit))
  }
}
