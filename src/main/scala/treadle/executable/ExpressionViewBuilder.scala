// See LICENSE for license details.

package treadle.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import treadle._
import RenderHelper.ExpressionHelper


import scala.collection.mutable

//noinspection ScalaUnusedSymbol
class ExpressionViewBuilder(
    symbolTable: SymbolTable,
    dataStore: DataStore,
    scheduler: Scheduler,
    validIfIsRandom: Boolean,
    blackBoxFactories: Seq[BlackBoxFactory]
)
  extends logger.LazyLogging {

  val expressionViews: mutable.HashMap[Symbol, ExpressionView] = new mutable.HashMap

  def getWidth(tpe: firrtl.ir.Type): Int = {
    tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ => throw new TreadleException(s"Unresolved width found in firrtl.ir.Type $tpe")
    }
  }

  def getWidth(expression: Expression): Int = {
    expression.tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ =>
        throw new TreadleException(
          s"Unresolved width found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  def getSigned(expression: Expression): Boolean = {
    expression.tpe match {
      case  _: UIntType    => false
      case  _: SIntType    => true
      case  ClockType      => false
      case _ =>
        throw new TreadleException(
          s"Unsupported type found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  // scalastyle:off
  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def processStatements(statement: firrtl.ir.Statement): Unit = {

      def binaryOps(opCode: PrimOp, args: Seq[Expression], tpe: Type): ExpressionView = {

        def getParameters(e: Expression) = (processExpression(e), getSigned(e), getWidth(e))

        val (arg1, arg1IsSigned, arg1Width) = getParameters(args.head)
        val (arg2, arg2IsSigned, arg2Width) = getParameters(args.tail.head)

        expression"$opCode($arg1, $arg2)"
      }

      def oneArgOneParamOps(
          op: PrimOp,
          expressions: Seq[Expression],
          ints: Seq[BigInt],
          tpe: firrtl.ir.Type
      ): ExpressionView = {
        val arg1 = processExpression(expressions.head)
        val arg1Width = getWidth(expressions.head)
        val isSigned = getSigned(expressions.head)
        val param1 = ints.head.toInt

        expression"$op($arg1, $param1)"
      }

      def oneArgTwoParamOps(
          op: PrimOp,
          expressions: Seq[Expression],
          ints: Seq[BigInt],
          tpe: firrtl.ir.Type
      ): ExpressionView = {
        val arg1 = processExpression(expressions.head)
        val arg2 = ints.head
        val arg3 = ints.tail.head

        expression"$op($arg1, $arg2, $arg3)"
      }

      def unaryOps(
          op: PrimOp,
          expressions: Seq[Expression],
          tpe: firrtl.ir.Type
      ): ExpressionView = {
        val arg1 = processExpression(expressions.head)

        expression"$op($arg1)"
      }

      /*
        * Process loFirrtl expression and return an executable result
        *
        * @param expression a loFirrtlExpression
        * @return
        */
      def processExpression(expression: Expression): ExpressionView = {

        val result: ExpressionView = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            val conditionClause = processExpression(condition)
            val trueClause = processExpression(trueExpression)
            val falseClause = processExpression(falseExpression)
            expression"Mux($conditionClause, $trueClause, $falseClause)"
          case WRef(name, _, _, _) =>
            expression"${symbolTable(expand(name))}"
          case subfield: WSubField =>
            expression"${symbolTable(expand(subfield.serialize))}"
          case subIndex: WSubIndex =>
            expression"${symbolTable(expand(subIndex.serialize))}"

          case ValidIf(condition, value, tpe) =>
            if(validIfIsRandom) {
              expression"ValidIf(${processExpression(condition)}, ${processExpression(value)}}"
            }
            else {
              expression"ValidIf(ignored)${processExpression(value)}"
            }
          case DoPrim(op, args, const, tpe) =>
            val v = op match {
              case Add => binaryOps(op, args, tpe)
              case Sub => binaryOps(op, args, tpe)
              case Mul => binaryOps(op, args, tpe)
              case Div => binaryOps(op, args, tpe)
              case Rem => binaryOps(op, args, tpe)

              case Eq  => binaryOps(op, args, tpe)
              case Neq => binaryOps(op, args, tpe)
              case Lt  => binaryOps(op, args, tpe)
              case Leq => binaryOps(op, args, tpe)
              case Gt  => binaryOps(op, args, tpe)
              case Geq => binaryOps(op, args, tpe)

              case Pad     => unaryOps(op, args, tpe)

              case AsUInt  => unaryOps(op, args, tpe)
              case AsSInt  => unaryOps(op, args, tpe)
              case AsClock => unaryOps(op, args, tpe)

              case Shl => oneArgOneParamOps(op, args, const, tpe)
              case Shr => oneArgOneParamOps(op, args, const, tpe)

              case Dshl => binaryOps(op, args, tpe)
              case Dshr => binaryOps(op, args, tpe)

              case Cvt => unaryOps(op, args, tpe)
              case Neg => unaryOps(op, args, tpe)
              case Not => unaryOps(op, args, tpe)

              case And => binaryOps(op, args, tpe)
              case Or  => binaryOps(op, args, tpe)
              case Xor => binaryOps(op, args, tpe)

              case Andr => unaryOps(op, args, tpe)
              case Orr =>  unaryOps(op, args, tpe)
              case Xorr => unaryOps(op, args, tpe)

              case Cat => binaryOps(op, args, tpe)

              case Bits => oneArgTwoParamOps(op, args, const, tpe)

              case Head => oneArgOneParamOps(op, args, const, tpe)
              case Tail => oneArgOneParamOps(op, args, const, tpe)
              case _ =>
                throw new Exception(s"processExpression:error: unhandled expression $expression")
            }
            v
          case UIntLiteral(value, IntWidth(width)) =>
            expression"$value.U"
          case SIntLiteral(value, IntWidth(width)) =>
            expression"$value.S"
          case _ =>
            throw new TreadleException(s"bad expression $expression")
        }
        result
      }

      statement match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processStatements(subStatement)
          }

        case con: Connect =>
          // if it's a register we use the name of its input side
          def renameIfRegister(name: String): String = {
            if (symbolTable.isRegister(name)) {
              SymbolTable.makeRegisterInputName(name)
            }
            else {
              name
            }
          }
          val lhsName = renameIfRegister(expand(con.loc.serialize))
          val view = processExpression(con.expr)
          expressionViews(symbolTable(lhsName)) = view

        case WDefInstance(info, instanceName, moduleName, _) =>
          val subModule = FindModule(moduleName, circuit)
          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          processModule(newPrefix, subModule, circuit)

          subModule match {
            case extModule: ExtModule =>
              val instanceSymbol = symbolTable(expand(instanceName))

              symbolTable.getBlackboxImplementation(instanceSymbol) match {
                case Some(implementation) =>
                  val instanceSymbol = symbolTable(expand(instanceName))

                  for (port <- extModule.ports) {
                    if (port.direction == Output) {
                      val portSymbol = symbolTable(expand(instanceName + "." + port.name))
                      val inputSymbols = implementation.outputDependencies(port.name).map { inputName =>
                        symbolTable(expand(instanceName + "." + inputName))
                      }
                      expressionViews(portSymbol) =
                        expression"blackbox[${instanceSymbol.name}](${inputSymbols.map(_.name)})"
                    }
                  }
                case _ =>
                  println(
                    s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
                      """was not matched with an implementation""")
              }
            case _ =>
            // not external module, it was processed above
          }

        case DefNode(info, name, expression) =>
          expressionViews(symbolTable(expand(name))) = processExpression(expression)

        case DefWire(info, name, tpe) =>

        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
          val expandedName = expand(name)
          val registerIn  = symbolTable(SymbolTable.makeRegisterInputName(expandedName))
          val registerOut = symbolTable(expandedName)
          expressionViews(registerOut) = expression"$registerIn"

        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
//          Memory.buildMemoryInternals(defMemory, expandedName, scheduler, compiler = this)
        case IsInvalid(info, expression) =>

        case Stop(info, ret, clockExpression, enableExpression) =>

        case Print(info, stringLiteral, argExpressions, clockExpression, enableExpression) =>

        case EmptyStmt =>
        case conditionally: Conditionally =>
          // logger.debug(s"got a conditionally $conditionally")
          throw new TreadleException(s"conditionally unsupported in engine $conditionally")
        case _ =>
          println(s"TODO: Unhandled statement $statement")
      }
    }
    // scalastyle:on

    myModule match {
      case module: firrtl.ir.Module =>
        processStatements(module.body)
      case extModule: ExtModule => // Look to see if we have an implementation for this
        logger.debug(s"got external module ${extModule.name} instance $modulePrefix")
      // all handling of an instance at the compiler stage occurs at a DefInstance above.
    }
  }

  // scalastyle:off cyclomatic.complexity
  def compile(circuit: Circuit, blackBoxFactories: Seq[BlackBoxFactory]): Unit = {
    val module = FindModule(circuit.main, circuit) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw TreadleException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw TreadleException(s"Top level module is not the right kind of module $x")
    }

    processModule("", module, circuit)
  }
}

object ExpressionViewBuilder {

  def getExpressionViews(
      symbolTable: SymbolTable,
      dataStore: DataStore,
      scheduler: Scheduler,
      validIfIsRandom: Boolean,
      circuit: Circuit,
      blackBoxFactories: Seq[BlackBoxFactory]): Map[Symbol, ExpressionView] = {
    val builder = new ExpressionViewBuilder(
      symbolTable, dataStore, scheduler, validIfIsRandom, blackBoxFactories)
    builder.compile(circuit, blackBoxFactories)
    builder.expressionViews.toMap
  }
}
