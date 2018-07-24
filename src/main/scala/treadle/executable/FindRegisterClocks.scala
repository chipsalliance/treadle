// See LICENSE for license details.

package treadle.executable

import firrtl.WRef
import firrtl.ir._

import scala.collection.mutable

object FindRegisterClocks {
  //scalastyle:off method.length cyclomatic.complexity
  def run(topModule: DefModule, circuit: Circuit, symbolTable: SymbolTable): mutable.HashSet[Symbol] = {

    val clockSymbols = new mutable.HashSet[Symbol]

    def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
      def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

      def processStatements(modulePrefix: String, circuit: Circuit, statement: firrtl.ir.Statement): Unit = {
        def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

        def getDrivingClock(clockExpression: Expression): Option[Symbol] = {

          clockExpression match {
            case WRef(clockName, _, _, _) =>
              for {
                clockSym <- symbolTable.get(expand(clockName))
                topClock <- symbolTable.findHighestClock(clockSym)
              } yield {
                topClock
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

          case DefRegister(info, name, _, clockExpression, _, _) =>

            getDrivingClock(clockExpression) match {
              case Some(clockSymbol) =>
                clockSymbols += clockSymbol

              case _ =>
            }

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
