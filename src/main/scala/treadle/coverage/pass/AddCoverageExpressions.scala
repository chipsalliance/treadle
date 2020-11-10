// SPDX-License-Identifier: Apache-2.0
package treadle.coverage.pass

import firrtl.{Namespace, Utils}
import firrtl.ir.{Circuit, Module, ExtModule, DefModule, IntWidth, NoInfo, Output, Port, Statement, UIntType}
import firrtl.passes.Pass
import firrtl.stage.TransformManager.TransformDependency

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
        val newPorts = newModule.ports ++ (for (_ <- 0 until ledger.nMux(module.name))
            yield Port(NoInfo, namespace.newName(coverageName), Output, UIntType(IntWidth(1))))

        //Add new ports to the module
        newModule match {
            case mod: Module => mod.copy(ports = newPorts)
            case extMod: ExtModule => extMod.copy(ports = newPorts)
            case _ => _
        }
    }

    /**
      * TODO: Traverse statements, find muxes and insert coverage ports there
      */
    def coverS(ledger: Ledger, namespace: Namespace)(s: Statement) : Statement = {
        s
    }
}

class Ledger {
    private var moduleName: Option[String] = None
    private val modules = mutable.Set[String]()
    private val moduleMuxMap = mutable.Map[String, Int]()

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