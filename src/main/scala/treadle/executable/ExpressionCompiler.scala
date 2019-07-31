// See LICENSE for license details.

package treadle.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import treadle._
import treadle.utils.FindModule

import scala.collection.mutable

class ExpressionCompiler(
    val symbolTable  : SymbolTable,
    val dataStore    : DataStore,
    scheduler        : Scheduler,
    validIfIsRandom  : Boolean,
    blackBoxFactories: Seq[ScalaBlackBoxFactory]
)
  extends logger.LazyLogging {

  case class ExternalInputParams(instance: ScalaBlackBox, portName: String)

  private val externalModuleInputs = new mutable.HashMap[Symbol, ExternalInputParams]

  def getWidth(tpe: firrtl.ir.Type): Int = {
    tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ => throw TreadleException(s"Unresolved width found in firrtl.ir.Type $tpe")
    }
  }

  def getWidth(expression: Expression): Int = {
    expression.tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ =>
        throw TreadleException(
          s"Unresolved width found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  def getSigned(expression: Expression): Boolean = {
    expression.tpe match {
      case  _: UIntType    => false
      case  _: SIntType    => true
      case  ClockType      => false
      case _ =>
        throw TreadleException(
          s"Unsupported type found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  def makeGet(source: Symbol): ExpressionResult = {
    source.dataSize match {
      case IntSize =>
        dataStore.GetInt(source.index)
      case LongSize =>
        dataStore.GetLong(source.index)
      case BigSize =>
        dataStore.GetBig(source.index)
    }
  }

  def makeGet(sourceName: String): ExpressionResult = {
    makeGet(symbolTable(sourceName))
  }

  def makeGetIndirect(memory: Symbol, data: Symbol, enable: Symbol, addr: Symbol): ExpressionResult = {
    data.dataSize match {
      case IntSize =>
        dataStore.GetIntIndirect(
          memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
        )
      case LongSize =>
        dataStore.GetLongIndirect(
          memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
        )
      case BigSize =>
        dataStore.GetBigIndirect(
          memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
        )
    }
  }

  //scalastyle:off cyclomatic.complexity method.length
  def makeAssigner(
    symbol                : Symbol,
    expressionResult      : ExpressionResult,
    conditionalClockSymbol: Option[Symbol] = None,
    info                  : Info
  ): Unit = {
    val assigner = (symbol.dataSize, expressionResult) match {
      case (IntSize,  result: IntExpressionResult)  =>
        dataStore.AssignInt(symbol, result.apply, info)

      case (IntSize,  result: LongExpressionResult) =>
          dataStore.AssignInt(symbol,  ToInt(result.apply).apply, info)

      case (IntSize,  result: BigExpressionResult)  =>
          dataStore.AssignInt(symbol,  ToInt(result.apply).apply, info)

      case (LongSize, result: IntExpressionResult)  => dataStore.AssignLong(symbol, ToLong(result.apply).apply, info)
      case (LongSize, result: LongExpressionResult) => dataStore.AssignLong(symbol, result.apply, info)
      case (LongSize, result: BigExpressionResult)  => dataStore.AssignLong(symbol, BigToLong(result.apply).apply, info)
      case (BigSize,  result: IntExpressionResult)  => dataStore.AssignBig(symbol,  ToBig(result.apply).apply, info)
      case (BigSize,  result: LongExpressionResult) => dataStore.AssignBig(symbol,  LongToBig(result.apply).apply, info)
      case (BigSize,  result: BigExpressionResult)  => dataStore.AssignBig(symbol,  result.apply, info)
      case (size, result) =>
        val expressionSize = result match {
          case _: IntExpressionResult => "Int"
          case _: LongExpressionResult => "Long"
          case _: BigExpressionResult => "Big"
        }

        throw TreadleException(
          s"Error:assignment size mismatch ($size)${symbol.name} <= ($expressionSize)$expressionResult")
    }

    conditionalClockSymbol match {
      case Some(clockSymbol) =>
        val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))
        addAssigner(ClockBasedAssigner(assigner, clockSymbol, prevClockSymbol, dataStore, PositiveEdge))
      case _ =>
        addAssigner(assigner)
    }
  }

  def addAssigner(assigner: Assigner): Unit = {
    val symbol = assigner.symbol
    externalModuleInputs.get(symbol) match {
      case Some(ExternalInputParams(instance, portName)) =>
        // if there's a black box listening to this add the ext module wrapper
        scheduler.addAssigner(symbol, dataStore.ExternalModuleInputAssigner(symbol, portName, instance, assigner))
      case _ =>
        // typical case
        scheduler.addAssigner(symbol, assigner)
    }
  }

  def makeIndirectAssigner(
    portSymbol       : Symbol,
    memorySymbol     : Symbol,
    memoryIndex      : Int,
    enableIndex      : Int,
    expressionResult : ExpressionResult,
    clock            : Symbol,
    info             : Info
  ): Unit = {

    def getIndex = dataStore.GetInt(memoryIndex).apply _
    def getEnable = {
      dataStore.GetInt(enableIndex).apply _
    }

    val assigner = (memorySymbol.dataSize, expressionResult) match {
      case (IntSize, result: IntExpressionResult) =>
        dataStore.AssignIntIndirect(portSymbol, memorySymbol, getIndex, getEnable, result.apply, info)
      case (LongSize, result: IntExpressionResult) =>
        dataStore.AssignLongIndirect(portSymbol, memorySymbol, getIndex, getEnable, ToLong(result.apply).apply, info)
      case (LongSize, result: LongExpressionResult) =>
        dataStore.AssignLongIndirect(portSymbol, memorySymbol, getIndex, getEnable, result.apply, info)
      case (BigSize, result: IntExpressionResult) =>
        dataStore.AssignBigIndirect(portSymbol, memorySymbol, getIndex, getEnable, ToBig(result.apply).apply, info)
      case (BigSize, result: LongExpressionResult) =>
        dataStore.AssignBigIndirect(portSymbol, memorySymbol, getIndex, getEnable, LongToBig(result.apply).apply, info)
      case (BigSize, result: BigExpressionResult) =>
        dataStore.AssignBigIndirect(portSymbol, memorySymbol, getIndex, getEnable, result.apply, info)
      case (size, result) =>
        val expressionSize = result match {
          case _: IntExpressionResult => "Int"
          case _: LongExpressionResult => "Long"
          case _: BigExpressionResult => "Big"
        }

        throw TreadleException(
          s"Error:assignment size mismatch ($size)${memorySymbol.name} <= ($expressionSize)$expressionResult")
    }
    addAssigner(assigner)
  }

  //scalastyle:off method.length
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

    def binaryOps(opCode: PrimOp, args: Seq[Expression], tpe: Type): ExpressionResult = {

      def getParameters(e: Expression) = (processExpression(e), getSigned(e), getWidth(e))

      val (arg1, _, arg1Width) = getParameters(args.head)
      val (arg2, _, arg2Width) = getParameters(args.tail.head)

      def handleIntResult(e1: IntExpressionResult, e2: IntExpressionResult): ExpressionResult = {
        opCode match {
          case Add => AddInts(e1.apply, e2.apply)
          case Sub => SubInts(e1.apply, e2.apply)
          case Mul => MulInts(e1.apply, e2.apply)
          case Div => DivInts(e1.apply, e2.apply)
          case Rem => RemInts(e1.apply, e2.apply)

          case Eq  => EqInts(e1.apply,  e2.apply)
          case Neq => NeqInts(e1.apply, e2.apply)
          case Lt  => LtInts(e1.apply,  e2.apply)
          case Leq => LeqInts(e1.apply, e2.apply)
          case Gt  => GtInts(e1.apply,  e2.apply)
          case Geq => GeqInts(e1.apply, e2.apply)

          case Dshl => DshlInts(e1.apply, e2.apply)
          case Dshr => DshrInts(e1.apply, e2.apply)

          case And => AndInts(e1.apply, e2.apply, arg1Width.max(arg2Width))
          case Or  => OrInts(e1.apply,  e2.apply, arg1Width.max(arg2Width))
          case Xor => XorInts(e1.apply, e2.apply, arg1Width.max(arg2Width))

          case Cat =>
            CatInts(e1.apply, arg1Width, e2.apply, arg2Width)

          case _ =>
            throw TreadleException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
        }
      }

      def handleLongResult(e1: LongExpressionResult, e2: LongExpressionResult): ExpressionResult = {
        opCode match {
          case Add => AddLongs(e1.apply, e2.apply)
          case Sub => SubLongs(e1.apply, e2.apply)
          case Mul => MulLongs(e1.apply, e2.apply)
          case Div => DivLongs(e1.apply, e2.apply)
          case Rem => RemLongs(e1.apply, e2.apply)

          case Eq  => EqLongs(e1.apply, e2.apply)
          case Neq => NeqLongs(e1.apply, e2.apply)
          case Lt  => LtLongs(e1.apply, e2.apply)
          case Leq => LeqLongs(e1.apply, e2.apply)
          case Gt  => GtLongs(e1.apply, e2.apply)
          case Geq => GeqLongs(e1.apply, e2.apply)

          case Dshl => DshlLongs(e1.apply, e2.apply)
          case Dshr => DshrLongs(e1.apply, e2.apply)

          case And  => AndLongs(e1.apply, e2.apply, arg1Width.max(arg2Width))
          case Or   => OrLongs(e1.apply, e2.apply, arg1Width.max(arg2Width))
          case Xor  => XorLongs(e1.apply, e2.apply, arg1Width.max(arg2Width))

          case Cat =>
            CatLongs(e1.apply, arg1Width, e2.apply, arg2Width)

          case _ =>
            throw TreadleException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
        }
      }

      def handleBigResult(e1: BigExpressionResult, e2: BigExpressionResult): ExpressionResult = {
        opCode match {
          case Add => AddBigs(e1.apply, e2.apply)
          case Sub => SubBigs(e1.apply, e2.apply)
          case Mul => MulBigs(e1.apply, e2.apply)
          case Div => DivBigs(e1.apply, e2.apply)
          case Rem => RemBigs(e1.apply, e2.apply)

          case Eq  => EqBigs(e1.apply, e2.apply)
          case Neq => NeqBigs(e1.apply, e2.apply)
          case Lt  => LtBigs(e1.apply, e2.apply)
          case Leq => LeqBigs(e1.apply, e2.apply)
          case Gt  => GtBigs(e1.apply, e2.apply)
          case Geq => GeqBigs(e1.apply, e2.apply)

          case Dshl => DshlBigs(e1.apply, e2.apply)
          case Dshr => DshrBigs(e1.apply, e2.apply)

          case And  => AndBigs(e1.apply, e2.apply, arg1Width.max(arg2Width))
          case Or   => OrBigs(e1.apply, e2.apply, arg1Width.max(arg2Width))
          case Xor  => XorBigs(e1.apply, e2.apply, arg1Width.max(arg2Width))

          case Cat =>
            CatBigs(e1.apply, arg1Width, e2.apply, arg2Width)

          case _ =>
            throw TreadleException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
        }
      }

      (DataSize(getWidth(tpe)), arg1, arg2) match {

        case (IntSize,  e1: IntExpressionResult, e2: IntExpressionResult) =>
          handleIntResult(e1, e2)

        case (IntSize, e1: LongExpressionResult, e2: IntExpressionResult) =>
          handleLongResult(e1, ToLong(e2.apply))
        case (IntSize, e1: BigExpressionResult, e2: IntExpressionResult) =>
          handleBigResult(e1, ToBig(e2.apply))

        case (IntSize, e1: IntExpressionResult, e2: LongExpressionResult) =>
          handleLongResult(ToLong(e1.apply), e2)
        case (IntSize, e1: LongExpressionResult, e2: LongExpressionResult) =>
          handleLongResult(e1, e2)
        case (IntSize, e1: BigExpressionResult, e2: LongExpressionResult) =>
          handleBigResult(e1, LongToBig(e2.apply))

        case (IntSize, e1: IntExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(ToBig(e1.apply), e2)
        case (IntSize, e1: LongExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(LongToBig(e1.apply), e2)
        case (IntSize, e1: BigExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(e1, e2)
        case (IntSize, _, _) =>
          throw TreadleException(
            s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head}) ($arg1, $arg2)")

        case (LongSize, e1: IntExpressionResult, e2: IntExpressionResult) =>
          handleLongResult(ToLong(e1.apply), ToLong(e2.apply))
        case (LongSize, e1: LongExpressionResult, e2: IntExpressionResult) =>
          handleLongResult(e1, ToLong(e2.apply))
        case (LongSize, e1: BigExpressionResult, e2: IntExpressionResult) =>
          handleBigResult(e1, ToBig(e2.apply))

        case (LongSize, e1: IntExpressionResult, e2: LongExpressionResult) =>
          handleLongResult(ToLong(e1.apply), e2)
        case (LongSize, e1: LongExpressionResult, e2: LongExpressionResult) =>
          handleLongResult(e1, e2)
        case (LongSize, e1: BigExpressionResult, e2: LongExpressionResult) =>
          handleBigResult(e1, LongToBig(e2.apply))

        case (LongSize, e1: IntExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(ToBig(e1.apply), e2)
        case (LongSize, e1: LongExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(LongToBig(e1.apply), e2)
        case (LongSize, e1: BigExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(e1, e2)
        case (LongSize, _, _) =>
          throw TreadleException(
            s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head}) ($arg1, $arg2)")

        case (BigSize, e1: IntExpressionResult, e2: IntExpressionResult) =>
          handleBigResult(ToBig(e1.apply), ToBig(e2.apply))
        case (BigSize, e1: LongExpressionResult, e2: IntExpressionResult) =>
          handleBigResult(LongToBig(e1.apply), ToBig(e2.apply))
        case (BigSize, e1: BigExpressionResult, e2: IntExpressionResult) =>
          handleBigResult(e1, ToBig(e2.apply))

        case (BigSize, e1: IntExpressionResult, e2: LongExpressionResult) =>
          handleBigResult(ToBig(e1.apply), LongToBig(e2.apply))
        case (BigSize, e1: LongExpressionResult, e2: LongExpressionResult) =>
          handleBigResult(LongToBig(e1.apply), LongToBig(e2.apply))
        case (BigSize, e1: BigExpressionResult, e2: LongExpressionResult) =>
          handleBigResult(e1, LongToBig(e2.apply))

        case (BigSize, e1: IntExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(ToBig(e1.apply), e2)
        case (BigSize, e1: LongExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(LongToBig(e1.apply), e2)
        case (BigSize, e1: BigExpressionResult, e2: BigExpressionResult) =>
          handleBigResult(e1, e2)

        case (BigSize, _, _) =>
          throw TreadleException(
            s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head}) ($arg1, $arg2)")

        case (_, _, _) =>
          throw TreadleException(
            s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head}) ($arg1, $arg2)")
      }
    }

    def oneArgOneParamOps(
      op: PrimOp,
      expressions: Seq[Expression],
      ints: Seq[BigInt],
      tpe: firrtl.ir.Type
    ): ExpressionResult = {
      val arg1 = processExpression(expressions.head)
      val arg1Width = getWidth(expressions.head)
      val param1 = ints.head.toInt

      arg1 match {
        case e1: IntExpressionResult =>
          op match {
            case Head => HeadInts(e1.apply, takeBits = param1, arg1Width)
            case Tail => TailInts(e1.apply, toDrop = param1, arg1Width)
            case Shl  => ShlInts(e1.apply, GetIntConstant(param1).apply)
            case Shr  => ShrInts(e1.apply, GetIntConstant(param1).apply)
          }
        case e1: LongExpressionResult =>
          op match {
            case Head => HeadLongs(e1.apply, takeBits = param1, arg1Width)
            case Tail => TailLongs(e1.apply, toDrop = param1, arg1Width)
            case Shl  => ShlLongs(e1.apply, GetLongConstant(param1).apply)
            case Shr  => ShrLongs(e1.apply, GetLongConstant(param1).apply)
          }
        case e1: BigExpressionResult =>
          op match {
            case Head => HeadBigs(e1.apply, takeBits = param1, arg1Width)
            case Tail => TailBigs(e1.apply, toDrop = param1, arg1Width)
            case Shl  => ShlBigs(e1.apply, GetBigConstant(param1).apply)
            case Shr  => ShrBigs(e1.apply, GetBigConstant(param1).apply)
          }
      }
    }

    def oneArgTwoParamOps(
      op: PrimOp,
      expressions: Seq[Expression],
      ints: Seq[BigInt],
      tpe: firrtl.ir.Type
    ): ExpressionResult = {
      val arg1 = processExpression(expressions.head)
      val arg2 = ints.head
      val arg3 = ints.tail.head
      val width = tpe match {
        case UIntType(IntWidth(n)) => n.toInt
        case SIntType(IntWidth(n)) => n.toInt
      }

      arg1 match {
        case e1: IntExpressionResult =>
          op match {
            case Bits => BitsInts(e1.apply, arg2.toInt, arg3.toInt, width)
          }
        case e1: LongExpressionResult =>
          op match {
            case Bits => BitsLongs(e1.apply, arg2.toInt, arg3.toInt, width)
          }
        case e1: BigExpressionResult =>
          op match {
            case Bits => BitsBigs(e1.apply, arg2.toInt, arg3.toInt, width)
          }
      }
    }

    def unaryOps(
      op: PrimOp,
      expressions: Seq[Expression],
      tpe: firrtl.ir.Type
    ): ExpressionResult = {
      val arg1 = processExpression(expressions.head)

      val width = tpe match {
        case UIntType(IntWidth(n)) => n.toInt
        case SIntType(IntWidth(n)) => n.toInt
        case ClockType             => 1
        case AsyncResetType        => 1
      }

      val sourceWidth = getWidth(expressions.head)

      arg1 match {
        case e1: IntExpressionResult =>
          op match {
            case Pad            => e1
            case AsUInt         => AsUIntInts(e1.apply, width)
            case AsSInt         => AsSIntInts(e1.apply, width)
            case AsClock        => e1
            case AsAsyncReset   => e1

            case Cvt            => e1
            case Neg            => NegInts(e1.apply)
            case Not            => NotInts(e1.apply, width)

            case Andr           => AndrInts(e1.apply, sourceWidth)
            case Orr            => OrrInts(e1.apply,  sourceWidth)
            case Xorr           => XorrInts(e1.apply, sourceWidth)
          }
        case e1: LongExpressionResult =>
          op match {
            case Pad            => e1
            case AsUInt         => AsUIntLongs(e1.apply, width)
            case AsSInt         => AsSIntLongs(e1.apply, width)
            case AsClock        => e1
            case AsAsyncReset   => e1

            case Cvt            => e1
            case Neg            => NegLongs(e1.apply)
            case Not            => NotLongs(e1.apply, width)

            case Andr           => AndrLongs(e1.apply, sourceWidth)
            case Orr            => OrrLongs(e1.apply,  sourceWidth)
            case Xorr           => XorrLongs(e1.apply, sourceWidth)
          }
        case e1: BigExpressionResult =>
          op match {
            case Pad            => e1
            case AsUInt         => AsUIntBigs(e1.apply, width)
            case AsSInt         => AsSIntBigs(e1.apply, width)
            case AsClock        => e1
            case AsAsyncReset   => e1

            case Cvt            => e1
            case Neg            => NegBigs(e1.apply)
            case Not            => NotBigs(e1.apply, width)

            case Andr           => AndrBigs(e1.apply, sourceWidth)
            case Orr            => OrrBigs(e1.apply,  sourceWidth)
            case Xorr           => XorrBigs(e1.apply, sourceWidth)
          }
      }
    }

    def processMux(
      condition: ExpressionResult,
      trueExpression: ExpressionResult,
      falseExpression: ExpressionResult
    ): ExpressionResult = {

      condition match {
        case c: IntExpressionResult =>
          (trueExpression, falseExpression) match {

            case (t: IntExpressionResult, f: IntExpressionResult) =>
              MuxInts(c.apply, t.apply, f.apply)
            case (t: IntExpressionResult, f: LongExpressionResult) =>
              MuxLongs(c.apply, ToLong(t.apply).apply, f.apply)
            case (t: IntExpressionResult, f: BigExpressionResult) =>
              MuxBigs(c.apply, ToBig(t.apply).apply, f.apply)

            case (t: LongExpressionResult, f: IntExpressionResult) =>
              MuxLongs(c.apply, t.apply, ToLong(f.apply).apply)
            case (t: LongExpressionResult, f: LongExpressionResult) =>
              MuxLongs(c.apply, t.apply, f.apply)
            case (t: LongExpressionResult, f: BigExpressionResult) =>
              MuxBigs(c.apply, LongToBig(t.apply).apply, f.apply)

            case (t: BigExpressionResult, f: IntExpressionResult) =>
              MuxBigs(c.apply, t.apply, ToBig(f.apply).apply)
            case (t: BigExpressionResult, f: LongExpressionResult) =>
              MuxBigs(c.apply, t.apply, LongToBig(f.apply).apply)
            case (t: BigExpressionResult, f: BigExpressionResult) =>
              MuxBigs(c.apply, t.apply, f.apply)

            case (a, b) =>
              throw TreadleException(s"Unhandled Mux($condition, $a, $b)")
          }
        case c =>
          throw TreadleException(s"Mux condition is not 1 bit $condition parsed as $c")
      }
    }

    /*
      * Process loFirrtl expression and return an executable result
      *
      * @param expression a loFirrtlExpression
      * @return
      */
    def processExpression(expression: Expression): ExpressionResult = {
      val result: ExpressionResult = expression match {
        case Mux(condition, trueExpression, falseExpression, _) =>
          processMux(
            processExpression(condition),
            processExpression(trueExpression),
            processExpression(falseExpression)
          )
        case WRef(name, _, _, _) =>
          makeGet(expand(name))
        case subfield: WSubField =>
          makeGet(expand(subfield.serialize))
        case subIndex: WSubIndex =>
          makeGet(expand(subIndex.serialize))

        case ValidIf(condition, value, tpe) =>
          if(validIfIsRandom) {
            processExpression(condition) match {
              case c: IntExpressionResult =>
                processExpression(value) match {
                  case t: IntExpressionResult =>
                    MuxInts(c.apply, t.apply, UndefinedInts(getWidth(tpe)).apply)
                  case t: LongExpressionResult =>
                    MuxLongs(c.apply, t.apply, UndefinedLongs(getWidth(tpe)).apply)
                  case t: BigExpressionResult =>
                    MuxBigs(c.apply, t.apply, UndefinedBigs(getWidth(tpe)).apply)
                  case _ =>
                    throw TreadleException(s"Mux condition is not 1 bit $condition parsed as $c")
                }
              case c =>
                throw TreadleException(s"Mux condition is not 1 bit $condition parsed as $c")
            }
          }
          else {
            processExpression(value)
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

            case AsUInt       => unaryOps(op, args, tpe)
            case AsSInt       => unaryOps(op, args, tpe)
            case AsClock      => unaryOps(op, args, tpe)
            case AsAsyncReset => unaryOps(op, args, tpe)

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
          DataSize(width) match {
            case IntSize  => GetIntConstant(value.toInt)
            case LongSize => GetLongConstant(value.toLong)
            case BigSize  => GetBigConstant(value)
          }
        case SIntLiteral(value, IntWidth(width)) =>
          DataSize(width) match {
            case IntSize  => GetIntConstant(value.toInt)
            case LongSize => GetLongConstant(value.toLong)
            case BigSize  => GetBigConstant(value)
          }
        case _ =>
          throw TreadleException(s"bad expression $expression")
      }
      result
    }

    statement match {
      case block: Block =>
        var statementNumber = 0
        while(statementNumber < block.stmts.length) {
          processStatements(modulePrefix, circuit, block.stmts(statementNumber))
          statementNumber += 1
        }

      case con: Connect =>
        // if it's a register we use the name of its input side

        val expandedName = expand(con.loc.serialize)
        if(symbolTable.isRegister(expandedName)) {
          val registerIn  = symbolTable(SymbolTable.makeRegisterInputName(expandedName))

          val processedExpression = processExpression(con.expr)

          makeAssigner(registerIn, processedExpression, info = con.info)
        }
        else {
          val assignedSymbol = symbolTable(expandedName)
          makeAssigner(assignedSymbol, processExpression(con.expr), info = con.info)

          if(assignedSymbol.firrtlType == ClockType) {
            //
            // If we are here then we need to add an assigner at the end of the cycle that records
            // the clocks state in the clock's prev state
            //
            val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(assignedSymbol))
            val prevClockAssigner = dataStore.AssignInt(
              prevClockSymbol, makeGet(assignedSymbol).asInstanceOf[IntExpressionResult].apply, info = con.info
            )
            scheduler.addEndOfCycleAssigner(prevClockAssigner)
          }
        }

      case WDefInstance(info, instanceName, moduleName, _) =>
        val subModule = FindModule(moduleName, circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
        processModule(newPrefix, subModule, circuit)

        subModule match {
          case extModule: ExtModule =>
            val instanceSymbol = symbolTable(expand(instanceName))

            symbolTable.getBlackboxImplementation(instanceSymbol) match {
              case Some(implementation) =>
                val instanceSymbol = symbolTable(expand(instanceName))

                implementation.setParams(extModule.params)

                for (port <- extModule.ports) {
                  if (port.direction == Output) {
                    val portSymbol = symbolTable(expand(instanceName + "." + port.name))
                    val inputSymbols = implementation.outputDependencies(port.name).map { inputName =>
                      symbolTable(expand(instanceName + "." + inputName))
                    }
                    val shim = dataStore.BlackBoxShim(port.name, portSymbol, inputSymbols, implementation)
                    makeAssigner(portSymbol, shim, info = info)
                  }
                  if (port.tpe == ClockType) {
                    val clockSymbol = symbolTable(expand(instanceName + "." + port.name))
                    val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))

                    val clockTransitionGetter = ClockTransitionGetter(clockSymbol, prevClockSymbol, dataStore)

                    val blackBoxCycler = BlackBoxCycler(
                      instanceSymbol, implementation, clockSymbol, clockTransitionGetter, info)

                    val drivingClockOption = symbolTable.findHighestClock(clockSymbol)

                    scheduler.addAssigner(instanceSymbol, blackBoxCycler)
                  }
                  else if(port.direction == Input) {
                    val portSymbol = symbolTable(expand(instanceName + "." + port.name))
                    externalModuleInputs(portSymbol) = ExternalInputParams(implementation, port.name)
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
        val symbol = symbolTable(expand(name))
        logger.debug(s"declaration:DefNode:${symbol.name}:${expression.serialize}")
        makeAssigner(symbol, processExpression(expression), info = info)
        if(symbol.firrtlType == ClockType) {
          //
          // If we are here then we need to add an assigner at the end of the cycle that records
          // the clocks state in the clock's prev state
          //
          val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(symbol))
          val prevClockAssigner = dataStore.AssignInt(
            prevClockSymbol, makeGet(symbol).asInstanceOf[IntExpressionResult].apply, info
          )
          scheduler.addEndOfCycleAssigner(prevClockAssigner)
        }

      case DefWire(_, name, _) =>
        logger.debug(s"declaration:DefWire:$name")

      case DefRegister(info, name, tpe, clockExpression, resetExpression, initExpression) =>

        logger.debug(s"declaration:DefRegister:$name")

        val registerOut = symbolTable(expand(name))
        val registerIn  = symbolTable(SymbolTable.makeRegisterInputName(registerOut.name))

        getDrivingClock(clockExpression) match {
          case Some(clockSymbol) =>
            val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))

            val clockValue     = dataStore.GetInt(clockSymbol.index)
            val prevClockValue = dataStore.GetInt(prevClockSymbol.index)
            val clockHigh   = GtInts(clockValue.apply, GetIntConstant(0).apply)
            val clockWasLow = EqInts(prevClockValue.apply, GetIntConstant(0).apply)
            val isPosEdge   = AndInts(clockHigh.apply, clockWasLow.apply, 1)

            val posEdgeMux = processMux(isPosEdge, makeGet(registerIn), makeGet(registerOut))

            if(resetExpression.tpe == AsyncResetType) {
              val resetValue = processExpression(resetExpression) match {
                case i: IntExpressionResult => i
                case _ =>
                  throw TreadleException(s"reset expression at $info, was not UInt<1>")
              }
              val asyncResetCondition = GtInts(resetValue.apply, GetIntConstant(0).apply)
              val asyncResetMux = processMux(asyncResetCondition, processExpression(initExpression), posEdgeMux)

              makeAssigner(registerOut, asyncResetMux, info = info)
            }
            else {
              //TODO: (Chick) We could use
              // makeAssigner(registerOut, posEdgeMux, info = info)
              // but it causes a slight performance regression
              val drivingClockOption = getDrivingClock(clockExpression)

              makeAssigner(registerOut, makeGet(registerIn), drivingClockOption, info = info)
            }
          case _ =>
            makeAssigner(registerOut, makeGet(registerIn), info = info)
        }


      case defMemory: DefMemory =>
        val expandedName = expand(defMemory.name)
        logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
        Memory.buildMemoryInternals(defMemory, expandedName, scheduler, compiler = this)

      case _: IsInvalid =>

      case stop @ Stop(info, returnValue, clockExpression, enableExpression) =>
        symbolTable.stopToStopInfo.get(stop) match {
          case Some(stopInfo) =>
            val intExpression = processExpression(enableExpression) match {
              case i : IntExpressionResult  => i
              case l : LongExpressionResult => LongToInt(l.apply)
              case b : BigExpressionResult  => ToInt(b.apply)
              case _ =>
                throw TreadleException(s"Error: stop $stop has unknown condition type")
            }

            getDrivingClock(clockExpression) match {
              case Some(clockSymbol) =>
                val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))

                val clockTransitionGetter = ClockTransitionGetter(clockSymbol, prevClockSymbol, dataStore)
                val stopOp = StopOp(
                  symbol = stopInfo.stopSymbol,
                  info = info,
                  returnValue = returnValue,
                  condition = intExpression,
                  hasStopped = symbolTable(StopOp.stopHappenedName),
                  dataStore = dataStore,
                  clockTransitionGetter
                )
                addAssigner(stopOp)
              case _ =>
                throw TreadleException(s"Could not find symbol for Stop $stop")
            }

          case _ =>
            throw TreadleException(s"Could not find symbol for Stop $stop")
        }

      case printf @ Print(info, stringLiteral, argExpressions, clockExpression, enableExpression) =>

        symbolTable.printToPrintInfo.get(printf) match {
          case Some(printInfo) =>
            val intExpression = processExpression(enableExpression) match {
              case i : IntExpressionResult  => i
              case l : LongExpressionResult => LongToInt(l.apply)
              case b : BigExpressionResult  => ToInt(b.apply)
              case _ =>
                throw TreadleException(s"Error: printf $printf has unknown condition type")
            }

            getDrivingClock(clockExpression) match {
              case Some(clockSymbol) =>
                val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))

                val clockTransitionGetter = ClockTransitionGetter(clockSymbol, prevClockSymbol, dataStore)
                val printOp = PrintfOp(
                  printInfo.printSymbol,
                  info, stringLiteral,
                  argExpressions.map { expression => processExpression(expression) },
                  clockTransitionGetter,
                  intExpression
                )
                addAssigner(printOp)
              case _ =>
                throw TreadleException(s"Error: no clock found for Print $printf")
            }


          case _ =>
            throw TreadleException(s"Could not find symbol for Print $printf")
        }

      case EmptyStmt =>

      case conditionally: Conditionally =>
        // logger.debug(s"got a conditionally $conditionally")
        throw TreadleException(s"conditionally unsupported in engine $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement $statement")
    }
  }
  // scalastyle:on

  def processTopLevelClocks(module: Module): Unit = {
    module.ports.foreach { port =>
      if(port.tpe == ClockType) {
        val clockSymbol = symbolTable(port.name)
        val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))
        val prevClockAssigner = dataStore.AssignInt(
          prevClockSymbol, makeGet(clockSymbol).asInstanceOf[IntExpressionResult].apply, info = NoInfo
        )
        scheduler.addEndOfCycleAssigner(prevClockAssigner)
      }
    }
  }

  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
    myModule match {
      case module: firrtl.ir.Module =>
        if(modulePrefix.isEmpty) {
          processTopLevelClocks(module)
        }
        processStatements(modulePrefix, circuit: Circuit, module.body)
      case extModule: ExtModule => // Look to see if we have an implementation for this
        logger.debug(s"got external module ${extModule.name} instance $modulePrefix")
        // all handling of an instance at the compiler stage occurs at a DefInstance above.
    }
  }

  // scalastyle:off cyclomatic.complexity
  def compile(circuit: Circuit, blackBoxFactories: Seq[ScalaBlackBoxFactory]): Unit = {
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
