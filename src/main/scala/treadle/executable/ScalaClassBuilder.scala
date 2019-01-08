// See LICENSE for license details.

package treadle.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import firrtl.transforms.DontCheckCombLoopsAnnotation
import treadle._
import treadle.chronometry.Timer
import treadle.utils.{BitMasks, FindModule, RemoveTempWires, ToLoFirrtl}

import scala.collection.mutable

trait SimplePokerPeeker {
  def poke(s: String, value: BigInt): Unit
  def peek(s:String): BigInt
  def expect(s:String, value: BigInt): Unit
  def step(steps: Int): Unit
  def cycles: Int
  def lastStopResult: Option[Int]
  def report()
}

//noinspection ScalaUnusedSymbol
class ScalaClassBuilder(
    symbolTable: SymbolTable,
    dataStore: DataStore,
    blackBoxFactories: Seq[ScalaBlackBoxFactory]
)
  extends logger.LazyLogging {

  val statements: mutable.HashMap[Symbol, String] = new mutable.HashMap
  val endOfCycleStatements = new mutable.ArrayBuffer[String]

  val intDataName  = "intArray"
  val longDataName = "longArray"
  val bigDataName  = "bigArray"

  def asVarName(symbol: Symbol): String = {
    s"`${symbol.name}`"
  }

  def dataStoreName(symbol: Symbol): String = {
    symbol.dataSize match {
      case IntSize  => intDataName
      case LongSize => longDataName
      case BigSize  => bigDataName
    }
  }

  def dataStoreRef(symbol: Symbol): String = {
    symbol.dataSize match {
      case IntSize  => s"$intDataName(${asVarName(symbol)})"
      case LongSize => s"$longDataName(${asVarName(symbol)})"
      case BigSize  => s"$bigDataName(${asVarName(symbol)})"
    }
  }

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

  // scalastyle:off
  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def processStatements(statement: firrtl.ir.Statement): Unit = {

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


      def binaryOps(opCode: String, args: Seq[Expression], tpe: Type): String = {

        def getParameters(e: Expression) = (processExpression(e), getSigned(e), getWidth(e))

        val (arg1, arg1IsSigned, arg1Width) = getParameters(args.head)
        val (arg2, arg2IsSigned, arg2Width) = getParameters(args.tail.head)

        if(opCode == "cat") {
          val mask1 = BitMasks.getBitMasksInts(arg1Width).allBitsMask
          s"((($arg1) & ${mask1}) << ${arg2Width}) | $arg2"
        }
        else {
          var s = s"""($arg1) $opCode ($arg2)"""
          if(Seq(">", ">=", "==", "<", "<=").contains(opCode)) {
            s = s"if($s) { 1 } else { 0 }"
          }
          s
        }
      }

      def oneArgOneParamOps(
          op: PrimOp,
          expressions: Seq[Expression],
          ints: Seq[BigInt],
          tpe: firrtl.ir.Type
      ): String = {
        val arg1 = processExpression(expressions.head)
        val arg1Width = getWidth(expressions.head)
        val isSigned = getSigned(expressions.head)
        val param1 = ints.head.toInt

        op match {


          case Shl => s"""$arg1 << $param1"""
          case Shr => s"""$arg1 >> $param1"""

          case Head =>
            val mask = (1 << param1) - 1
            val shift = arg1Width - param1
            s"""(($arg1) >> $shift) & ${mask}"""

          case Tail =>
            val mask: Int = (1 << (arg1Width - param1)) - 1

            s"""($arg1) & ${mask}"""
        }
      }

      def oneArgTwoParamOps(
          op: PrimOp,
          expressions: Seq[Expression],
          ints: Seq[BigInt],
          tpe: firrtl.ir.Type
      ): String = {
        val arg1 = processExpression(expressions.head)
        val arg2 = ints.head.toInt
        val arg3 = ints.tail.head.toInt

        val mask = (1 << ((arg2 - arg3) + 1)) - 1

        s"""(($arg1) >> ${arg3}) & ${mask}"""
      }

      def unaryOps(
          op: PrimOp,
          expressions: Seq[Expression],
          tpe: firrtl.ir.Type
      ): String = {
        val arg1 = processExpression(expressions.head)
        val width = getWidth(tpe)

        op match {
          case Pad     => arg1
          case Cvt     => arg1

          case Not =>
            val mask = BitMasks.getBitMasksInts(width).allBitsMask
            s"(~ $arg1) & $mask"

          case AsUInt  =>
            val bitMasks = BitMasks.getBitMasksInts(width)

            s"""
               |($arg1) & ${bitMasks.allBitsMask}
             """.stripMargin
          case AsSInt  =>
            val bitMasks = BitMasks.getBitMasksInts(width)
            s"""
               |    if(($arg1) < 0) {
               |      $arg1
               |    }
               |    else {
               |      if(($arg1) & ${bitMasks.msbMask} > 0) {
               |        (($arg1) & ${bitMasks.allBitsMask}) - ${bitMasks.nextPowerOfTwo}
               |      }
               |      else {
               |        ($arg1) & ${bitMasks.allBitsMask}
               |      }
               |    }
             """.stripMargin
          case AsClock => arg1

          //TODO: SCALABUILD
//          case Cvt => unaryOps(op, args, tpe)
//          case Neg => unaryOps(op, args, tpe)
//          case Not => unaryOps(op, args, tpe)
//
//          case Andr => unaryOps(op, args, tpe)
//          case Orr =>  unaryOps(op, args, tpe)
//          case Xorr => unaryOps(op, args, tpe)

        }
      }

      /*
        * Process loFirrtl expression and return an executable result
        *
        * @param expression a loFirrtlExpression
        * @return
        */
      def processExpression(expression: Expression): String = {

        val result: String = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            val conditionClause = processExpression(condition)
            val trueClause = processExpression(trueExpression)
            val falseClause = processExpression(falseExpression)
            s"""if(($conditionClause) > 0) { $trueClause } else { $falseClause }
             """.stripMargin
          case WRef(name, _, _, _) =>
            dataStoreRef(symbolTable(expand(name)))
          case subfield: WSubField =>
            dataStoreRef(symbolTable(expand(subfield.serialize)))
          case subIndex: WSubIndex =>
            dataStoreRef(symbolTable(expand(subIndex.serialize)))

          case ValidIf(condition, value, tpe) =>
            //TODO: SCALABUILD

//          if(validIfIsRandom) {
//              expression"ValidIf(${processExpression(condition)}, ${processExpression(value)}}"
//            }
//            else {
//              expression"ValidIf(ignored)${processExpression(value)}"
//            }
            s"${value}"
          case DoPrim(op, args, const, tpe) =>
            val v = op match {
              case Add => binaryOps("+", args, tpe)
              case Sub => binaryOps(opCode = "-", args, tpe)
              case Mul => binaryOps(opCode = "*", args, tpe)
              case Div => binaryOps(opCode = "/", args, tpe)
              case Rem => binaryOps(opCode = "%", args, tpe)

              case Eq  => binaryOps(opCode = "==", args, tpe)
              case Neq => binaryOps(opCode = "!=", args, tpe)
              case Lt  => binaryOps(opCode = "<", args, tpe)
              case Leq => binaryOps(opCode = "<=", args, tpe)
              case Gt  => binaryOps(opCode = ">", args, tpe)
              case Geq => binaryOps(opCode = ">=", args, tpe)

              case Pad     => unaryOps(op, args, tpe)

              case AsUInt  => unaryOps(op, args, tpe)
              case AsSInt  => unaryOps(op, args, tpe)
              case AsClock => unaryOps(op, args, tpe)

              case Shl => oneArgOneParamOps(op, args, const, tpe)
              case Shr => oneArgOneParamOps(op, args, const, tpe)

              case Dshl => binaryOps(opCode = "<<", args, tpe)
              case Dshr => binaryOps(opCode = ">>", args, tpe)

              case Cvt => unaryOps(op, args, tpe)
              case Neg => unaryOps(op, args, tpe)
              case Not => unaryOps(op, args, tpe)

              case And => binaryOps(opCode = "&", args, tpe)
              case Or  => binaryOps(opCode = "|", args, tpe)
              case Xor => binaryOps(opCode = "^", args, tpe)

              case Andr => unaryOps(op, args, tpe)
              case Orr =>  unaryOps(op, args, tpe)
              case Xorr => unaryOps(op, args, tpe)

              case Cat => binaryOps(opCode = "cat", args, tpe)

              case Bits => oneArgTwoParamOps(op, args, const, tpe)

              case Head => oneArgOneParamOps(op, args, const, tpe)
              case Tail => oneArgOneParamOps(op, args, const, tpe)
              case _ =>
                throw new Exception(s"processExpression:error: unhandled expression $expression")
            }
            v
          case UIntLiteral(value, IntWidth(width)) =>
            DataSize(width) match {
              case IntSize  => s"${value.toInt}"
              case LongSize => s"${value.toLong}L"
              case BigSize  => s"""BigInt("$value", 10)"""
            }
          case SIntLiteral(value, IntWidth(width)) =>
            DataSize(width) match {
              case IntSize  => s"${value.toInt}"
              case LongSize => s"${value.toLong}L"
              case BigSize  => s"""BigInt("$value", 10)"""
            }
          case _ =>
            throw TreadleException(s"bad expression $expression")
        }
        result
      }

      statement match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processStatements(subStatement)
          }

        case con: Connect =>
          val expandedName = expand(con.loc.serialize)
          if(!symbolTable.isRegister(expandedName)) {
            val assignedSymbol = symbolTable(expandedName)
            statements(symbolTable(expandedName)) =
                    s"""
                       |    ${dataStoreRef(assignedSymbol)} = ${processExpression(con.expr)}
                     """.stripMargin
          }
          else {
            val registerOut = symbolTable(expandedName)
            val registerIn = symbolTable(SymbolTable.makeRegisterInputName(expandedName))

            val processedExpression = processExpression(con.expr)

            statements(registerIn) =
                    s"""
                       |    ${dataStoreRef(registerIn)} = ${processExpression(con.expr)}
                     """.stripMargin

          }

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
                      //TODO: SCALABUILD: FIGURE OUT HOW TO ADD BLACK BOXES
//                      statements(portSymbol) =
//                        expression"blackbox[${instanceSymbol.name}](${inputSymbols.map(_.name)})"
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
          val lhsName = expand(name)
          val lhsSymbol = symbolTable(lhsName)
          statements(symbolTable(lhsName)) = s"${dataStoreRef(lhsSymbol)} = ${processExpression(expression)}"

        case DefWire(info, name, tpe) =>

        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
          val registerName = expand(name)
          val registerOut = symbolTable(registerName)
          val registerInputName = SymbolTable.makeRegisterInputName(registerName)
          val registerIn = symbolTable(registerInputName)
          val clockSymbol = symbolTable.registerToClock(registerOut)
          val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))

          statements(registerOut) =
                  s"""
                     |    if(${intDataName}(${asVarName(clockSymbol)}) > 0 && ${intDataName}(${asVarName(prevClockSymbol)}) == 0) {
                     |      ${dataStoreName(registerOut)}(${asVarName(registerOut)}) = ${dataStoreRef(registerIn)}
                     |    }
                     """.stripMargin

        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")

          //TODO: SCALABUILD
          // Memory.buildMemoryExpressions(defMemory, expandedName, scheduler, statements)
        case IsInvalid(info, expression) =>

        case stop @ Stop(info, ret, clockExpression, enableExpression) =>
          val stopSymbol = symbolTable.stopToStopInfo(stop).stopSymbol
          getDrivingClock(clockExpression).foreach { clockSymbol =>
            val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))
            statements(stopSymbol) =
                    s"""
                       |    if(${intDataName}(${asVarName(clockSymbol)}) > 0 && ${intDataName}(${asVarName(prevClockSymbol)}) == 0) {
                       |      if((${processExpression(enableExpression)}) > 0) {
                       |        lastStopResult = Some($ret)
                       |        println("Dang I'm stopped")
                       |      }
                       |    }
                     """.stripMargin

          }

        case print @ Print(info, stringLiteral, argExpressions, clockExpression, enableExpression) =>
          //TODO: SCALABUILD
//          statements(symbolTable.printToPrintInfo(print).printSymbol) = processExpression(enableExpression)

        case EmptyStmt =>
        case conditionally: Conditionally =>
          // logger.debug(s"got a conditionally $conditionally")
          throw TreadleException(s"conditionally unsupported in engine $conditionally")
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

  // scalastyle:off cyclomatic.complexity method.length
  def compile(circuit: Circuit, blackBoxFactories: Seq[ScalaBlackBoxFactory]): String = {
    val pw = new mutable.StringBuilder()

    def addClockPrevStatements(): Unit = {
      symbolTable.registerToClock.values.toSet.foreach { clockSymbol: Symbol =>
        val prevClockSymbol = symbolTable(SymbolTable.makePreviousValue(clockSymbol))
        pw ++= s"    ${dataStoreRef(prevClockSymbol)} = ${dataStoreRef(clockSymbol)}\n"
      }
    }

    def addNamedIndices(): Unit = {
      val validSymbols = symbolTable.nameToSymbol.values.filter(_.index >= 0)
      validSymbols.foreach { symbol =>
        pw ++= s"  val `${symbol.name}` = ${symbol.index}\n"
      }
      pw ++= "\n"

      val mapString = validSymbols.map { symbol =>
        s"""    "${symbol.name}" -> ${symbol.index}"""
      }.mkString("  val dataIndices = Map(\n", ",\n", "  )")

      pw ++= mapString
      pw ++= "\n"
    }

    def addPeekPoke(): Unit = {
      val s = s"""
         |  def poke(s: String, value: BigInt): Unit = {
         |    intArray(dataIndices(s)) = value.toInt
         |  }
         |
         |  def peek(s: String): BigInt = {
         |    BigInt(intArray(dataIndices(s)))
         |  }
         |
         |  def expect(s: String, value: BigInt): Unit = {
         |    if(BigInt(intArray(dataIndices(s))) != value) {
         |      throw new Exception(s"Expect failure expect(" + s + ", " + value + ") instead got " + value)
         |    }
         |  }
         |
         |  def step(steps: Int): Unit = {
         |    var step = 0
         |    while(step < steps) {
         |      update()
         |      intArray(dataIndices("clock")) = 1
         |      update()
         |      intArray(dataIndices("clock")) = 0
         |      step += 1
         |      cycles += 1
         |    }
         |  }
         |
         |  def report(): Unit = {}
         |
       """.stripMargin
      pw ++= s
      pw ++= "\n"
    }


    pw ++= s"""
         |import treadle.executable.SimplePokerPeeker
         |
         |class ${circuit.main}ScalaImpl extends SimplePokerPeeker {
         |  val intArray = Array.fill(${dataStore.intData.length})(0)
         |  val longArray = Array.fill(${dataStore.longData.length})(0L)
         |  val bigArray = Array.fill(${dataStore.bigData.length})(BigInt(0))
         |  var cycles = 0
         |  var lastStopResult: Option[Int] = None
         |
         |
       """.stripMargin

    addNamedIndices()

    addPeekPoke()

    val module = FindModule(circuit.main, circuit) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw TreadleException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw TreadleException(s"Top level module is not the right kind of module $x")
    }

    processModule("", module, circuit)

    pw ++= s"\n  def update(): Unit = {\n"

    statements.keys.toList.sortBy(_.cardinalNumber).foreach { key =>
      pw ++= "    " + statements(key) + "\n"
    }

    addClockPrevStatements()

    pw ++= s"  }\n"

    pw ++= "}\n"

    pw ++= "\nnew GCDScalaImpl\n"

    pw.toString
  }
}

object ScalaClassBuilder {
  def makeScalaSource(
    firrtlText:        String,
    optionsManager:    TreadleOptionsManager
  ): String = {

    val treadleOptions: TreadleOptions = optionsManager.treadleOptions

    val ast = firrtl.Parser.parse(firrtlText.split("\n").toIterator)
    val blackBoxFactories: Seq[ScalaBlackBoxFactory] = treadleOptions.blackBoxFactories
    val timer = new Timer

    val loweredAst: Circuit = if(treadleOptions.lowCompileAtLoad) {
      if(treadleOptions.allowCycles) {
        optionsManager.firrtlOptions = optionsManager.firrtlOptions.copy(
          annotations = optionsManager.firrtlOptions.annotations :+ DontCheckCombLoopsAnnotation
        )
      }
            val lowered = ToLoFirrtl.lower(ast, optionsManager)
            (new RemoveTempWires).execute(CircuitState(lowered, LowForm)).circuit
//      ToLoFirrtl.lower(ast, optionsManager)
    } else {
      ast
    }

    if(treadleOptions.showFirrtlAtLoad) {
      println("LoFirrtl" + "=" * 120)
      println(loweredAst.serialize)
    }

    val symbolTable: SymbolTable = timer("Build Symbol Table") {
      SymbolTable(loweredAst, blackBoxFactories, treadleOptions.allowCycles)
    }

    val dataStoreAllocator = new DataStoreAllocator

    symbolTable.allocateData(dataStoreAllocator)

    val dataStore = DataStore(treadleOptions.rollbackBuffers, dataStoreAllocator)

    val classBuilder = new ScalaClassBuilder(symbolTable, dataStore, blackBoxFactories)
    classBuilder.compile(loweredAst, Seq.empty)
  }
}


