// See LICENSE for license details.

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
    def insert(newStmts: mutable.ArrayBuffer[Statement], namespace: Namespace, info: Info, clockExpression: Expression)
            (a: Expression): Expression = {
      if (Utils.kind(a) == RegKind) {
        val newName = namespace.newTemp
        val wref = WRef(newName, a.tpe, NodeKind, MALE)
        newStmts += DefRegister(info, newName, a.tpe, clockExpression, UIntLiteral(0), UIntLiteral(0))
        newStmts += Connect(info, wref, a)
        wref
      } else {
        a
      }
    }

    def fixPrintsStmt(namespace: firrtl.Namespace)
            (s: Statement): Statement = s.mapStmt(fixPrintsStmt(namespace)) match {
      case s: Stop =>
        val newStmts = mutable.ArrayBuffer[Statement]()
        val newStop = s.mapExpr(insert(newStmts, namespace, s.info, s.clk))
        Block(newStmts :+ newStop)
      case p: Print =>
        val newStmts = mutable.ArrayBuffer[Statement]()
        val newPrint = p.mapExpr(insert(newStmts, namespace, p.info, p.clk))
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
