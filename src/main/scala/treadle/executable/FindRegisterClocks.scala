// See LICENSE for license details.

package treadle.executable

import firrtl.{WDefInstance, WRef}
import firrtl.ir._
import treadle.utils.FindModule

import scala.collection.mutable

/**
  * This transform finds all the the things that have driving clocks, registers, printfs and stops
  * and records the clock they will be driven by. This is important when the driving clock is
  * derived from an asClock primitive. Without this process changes to those wires would not cause a
  * these things to get triggered on the rising edge.
  */
object FindRegisterClocks {
  //scalastyle:off method.length cyclomatic.complexity
  def run(topModule: DefModule, circuit: Circuit, symbolTable: SymbolTable): mutable.HashSet[Symbol] = {

    val clockSymbols = new mutable.HashSet[Symbol]

    def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
      def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

      def processStatements(modulePrefix: String, circuit: Circuit, statement: firrtl.ir.Statement): Unit = {
        def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

        def recordDrivingClock(clockExpression: Expression): Unit = {

          clockExpression match {
            case WRef(clockName, _, _, _) =>
              for {
                clockSym <- symbolTable.get(expand(clockName))
                topClock <- symbolTable.findHighestClock(clockSym)
              } {
                clockSymbols += topClock
              }
            case _ =>
              None
          }
        }

        statement match {
          case block: Block =>
            var statementNumber = 0
            while(statementNumber < block.stmts.length) {
              processStatements(modulePrefix, circuit, block.stmts(statementNumber))
              statementNumber += 1
            }


          case WDefInstance(_, instanceName, moduleName, _) =>
            val subModule = FindModule(moduleName, circuit)
            val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
            processModule(newPrefix, subModule, circuit)

          case DefRegister(_, _, _, clockExpression, _, _) => recordDrivingClock(clockExpression)

          case Print(_, _, _, clockExpression, _)          => recordDrivingClock(clockExpression)

          case Stop(_, _, clockExpression, _)              => recordDrivingClock(clockExpression)

          case _ =>
        }
      }

      myModule match {
        case module: firrtl.ir.Module =>
          processStatements(modulePrefix, circuit: Circuit, module.body)
        case _ =>
      }
    }

    processModule(modulePrefix = "", topModule, circuit)

    clockSymbols
  }

}
