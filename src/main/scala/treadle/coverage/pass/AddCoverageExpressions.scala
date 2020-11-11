// SPDX-License-Identifier: Apache-2.0

package treadle.coverage.pass

import firrtl.Namespace
import firrtl.PrimOps.Not
import firrtl.ir.{Block, Circuit, Connect, DefModule, DoPrim, Expression, ExtModule, HasInfo, Module, Mux, Statement}
import firrtl.passes.Pass
import firrtl.stage.TransformManager.TransformDependency
import treadle.coverage.{CoverageExpressionInfo, Ledger}

import scala.collection.mutable

/**
  * Adds additional coverage statements to the Low FIRRTL source at every mux location.
  * These statements can then be used to gather statement coverage information from inside of
  */
object AddCoverageExpressions extends Pass {
    final val coverageName: String = "coverageValidator"

    override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

    override def name = "Coverage!"

    /**
      * Run the coverage extension on every module
      * @param c the circuit on with we want to add the coverage extensions
      * @return the newly modified version of the circuit including coverage validators
      */
    override def run(c: Circuit): Circuit = {
        val ledger = new Ledger()
        c.copy(modules = c.modules map coverM(ledger))
    }

    /**
      * Run coverage on every statement and then add additional coverage statements
      */
    private def coverM(ledger: Ledger)(module: DefModule): DefModule = {
        val namespace = Namespace(module)
        val newModule = module mapStmt coverS(ledger, namespace)
        val newPorts = newModule.ports ++ ledger.ports

        //Set the module name in the ledger
        ledger.setModuleName(module.name)

        //Add new ports to the module
        newModule match {
            case mod: Module => mod.copy(ports = newPorts)
            case extMod: ExtModule => extMod.copy(ports = newPorts)
        }
    }

    /**
      * Traverse statements, find muxes and insert coverage expressions there
      */
    private def coverS(ledger: Ledger, namespace: Namespace)(s: Statement) : Statement = {
        val block = mutable.ArrayBuffer[Statement]()
        s match {
            case s: HasInfo =>
                val newStmt = s.mapExpr(coverE(ledger, namespace, block))
                block.length match {
                    case 0 => newStmt
                    case _ => Block(block :+ newStmt)
                }
            case s => s.mapStmt(coverS(ledger, namespace))
        }
    }

    private def coverE(ledger: Ledger, namespace: Namespace, block: mutable.ArrayBuffer[Statement])(e: Expression): Expression =
        e.mapExpr(coverE(ledger, namespace, block)) match {
            //Look for muxes, if we find one, add the two coverage statements
            case Mux(cond, _, _, tpe) =>
                ledger.foundMux()

                //Create the two new ports that we will use as coverage validators
                val ref1 = ledger.addCoveragePort(namespace)
                val ref2 = ledger.addCoveragePort(namespace)
                block += Connect(CoverageExpressionInfo, ref1, cond)
                block += Connect(CoverageExpressionInfo, ref2, DoPrim(Not, Seq(cond), Seq(), tpe))
                e

            //We don't care about non-mux expressions
            case notmux => notmux
        }
}
