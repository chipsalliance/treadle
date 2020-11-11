// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.Namespace
import firrtl.ir.{Info, IntWidth, MultiInfo, Output, Port, Reference, UIntType}
import treadle.coverage.pass.AddCoverageExpressions.coverageName

import scala.collection.mutable

package object coverage {
    /**
      * Used to keep track of coverage validators
      */
    abstract class CoverageInfo extends Info {
        override def toString: String = "Coverage Validator"
        override def ++(that: Info): Info = that match {
            case CoverageExpressionInfo => MultiInfo(Seq(this, that))
            case CoveragePortInfo => MultiInfo(Seq(this, that))
            case _ => this
        }
    }

    case object CoverageExpressionInfo extends CoverageInfo
    case object CoveragePortInfo extends CoverageInfo

    /**
      * Keeps track of module and coverage related information during the firrtl pass
      */
    class Ledger {
        private var moduleName: Option[String] = None
        private val modules = mutable.Set[String]()
        private val moduleMuxMap = mutable.Map[String, Int]()
        private val coveragePorts = mutable.ArrayBuffer[Port]()

        /**
          * Retrieves a sequence containing all of the coverage ports
          */
        def ports: Seq[Port] = coveragePorts

        /**
          * Creates and recors a new coverage validation port
          * @param namespace the namespace used to generate a valid port name
          * @return a reference to the newly created port
          */
        def addCoveragePort(namespace: Namespace): Reference = {
            val port = Port(CoveragePortInfo, namespace.newName(coverageName), Output, UIntType(IntWidth(1)))
            coveragePorts += port
            Reference(port)
        }

        /**
          * Records the location of a newly detected mux
          */
        def foundMux(): Unit = moduleName match {
            case None       => sys.error("Module name not defined in Ledger!")
            case Some(name) => moduleMuxMap(name) = moduleMuxMap.getOrElse(name, 0) + 1
        }

        /**
          * Records the names of the different modules
          * @param myName the name we want to record
          */
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

        /**
          * Serializes the ledger (for testing)
          * @return a string representation of the ledger's contents
          */
        def serialize: String = {
            modules.map { myName =>
                s"$myName => ${nMux(myName)} muxes!"
            }.mkString("\n")
        }
    }

}
