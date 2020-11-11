// SPDX-License-Identifier: Apache-2.0
package treadle.coverage.pass

import firrtl.{Namespace, Utils}
import firrtl.ir.{Block, Circuit, Connect, DefModule, Expression, ExtModule, HasInfo, Info, IntWidth, Module, Mux,
    NoInfo, Output, Port, Reference, Statement, UIntType}
import firrtl.passes.Pass
import firrtl.stage.TransformManager.TransformDependency
import treadle.coverage.pass.AddCoverageExpressions.coverageName

import scala.collection.mutable

/**
  * Adds additional coverage statements to the Low FIRRTL source at every mux location.
  * These statements can then be used to gather statement coverage information from inside of
  */
object AddCoverageExpressions extends Pass {
    final val coverageName: String = "coverageValidator"

    override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

    override def name = "Coverage!"

    /** Run coverage on every module **/
    override def run(c: Circuit): Circuit = {
        val ledger = new Ledger()
        c.copy(modules = c.modules map coverM(ledger))
    }

    /**
      * Run coverage on every statement and then add additional coverage statements
      */
    def coverM(ledger: Ledger)(module: DefModule): DefModule = {
        val namespace = Namespace(module)
        val newModule = module mapStmt coverS(ledger, namespace)
        val newPorts = newModule.ports ++ ledger.ports

        //Add new ports to the module
        newModule match {
            case mod: Module => mod.copy(ports = newPorts)
            case extMod: ExtModule => extMod.copy(ports = newPorts)
        }
    }

    /**
      * TODO: Traverse statements, find muxes and insert coverage ports there
      */
    def coverS(ledger: Ledger, namespace: Namespace)(s: Statement) : Statement = {
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

    def coverE(ledger: Ledger, namespace: Namespace, block: mutable.ArrayBuffer[Statement])(e: Expression): Expression =
        e.mapExpr(coverE(ledger, namespace, block)) match {
            //Look for muxes, if we find one, add a coverage statement
            case Mux(cond, _, _, _) =>
                ledger.foundMux()
                val ref1 = ledger.addCoveragePort(namespace)
                val ref2 = ledger.addCoveragePort(namespace)
                block += Connect(NoInfo, ref1, cond)
                //block += Connect(NoInfo, ref2, notCond)
                e

            case notmux => notmux
        }
}

class Ledger {
    private var moduleName: Option[String] = None
    private val modules = mutable.Set[String]()
    private val moduleMuxMap = mutable.Map[String, Int]()
    private val coveragePorts = mutable.ArrayBuffer[Port]()
    private var curCovName: Int = 0

    def ports: Seq[Port] = coveragePorts

    def nextCoveragePort: Port =
        if(coveragePorts.length <= curCovName) throw new IllegalAccessException("No more names to use")
        else coveragePorts({
                val oldName = curCovName
                curCovName += 1
                oldName
            })

    def addCoveragePort(namespace: Namespace): Reference = {
        val port = Port(NoInfo, namespace.newName(coverageName), Output, UIntType(IntWidth(1)))
        coveragePorts += port
        Reference(port)
    }


    def foundMux(): Unit = moduleName match {
        case None       => sys.error("Module name not defined in Ledger!")
        case Some(name) => moduleMuxMap(name) = moduleMuxMap.getOrElse(name, 0) + 1
    }

    def getModuleName: String = moduleName match {
        case None       => Utils.error("Module name not defined in Ledger!")
        case Some(name) => name
    }

    def setModuleName(myName: String): Unit = {
        modules += myName
        moduleName = Some(myName)
    }

    /**
      * Counts the total number of muxes in our circuit
      */
    def totalMuxes: Int = moduleMuxMap.foldLeft(0)(_ + _._2)

    /**
      * Counts the number of muxes in a given module
      */
    def nMux(module: String) : Int = moduleMuxMap.getOrElse(module, 0)

    def serialize: String = {
        modules.map { myName =>
            s"$myName => ${nMux(myName)} muxes!"
        }.mkString("\n")
    }
}