// See LICENSE for license details.
package treadle

import java.io.{File, PrintWriter}

import treadle.vcd.VCD
import logger.Logger

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.{Terminal, TerminalFactory}
import scala.tools.jline.console.completer._
import collection.JavaConverters._
import scala.io.Source
import scala.util.matching.Regex

abstract class Command(val name: String) {
  def run(args: Array[String]): Unit
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(new ArgumentCompleter(
      new StringsCompleter({name})
    ))
  }
}

class TreadleRepl(val optionsManager: InterpreterOptionsManager with HasReplConfig) {
  val replConfig: ReplConfig = optionsManager.replConfig
  val interpreterOptions: InterpreterOptions = optionsManager.interpreterOptions

  treadle.random.setSeed(interpreterOptions.randomSeed)

  val terminal: Terminal = TerminalFactory.create()
  val console = new ConsoleReader
  private val historyPath = "~/.firrtl_repl_history".replaceFirst("^~",System.getProperty("user.home"))
  val historyFile = new File(historyPath)
  if(! historyFile.exists()) {
    println(s"creating ${historyFile.getName}")
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)

  history.load(historyFile)
  console.setHistory(history)

  var currentInterpreterOpt: Option[ExecutionEngine] = None

  def interpreter: ExecutionEngine = currentInterpreterOpt.get
  var args = Array.empty[String]
  var done = false

  var inScript = false
  val scriptFactory = ScriptFactory(this)
  var currentScript: Option[Script] = None
  val IntPattern: Regex = """(-?\d+)""".r

  var currentSymbols: String = ""

  var currentVcdScript: Option[VCD] = None
  var replVcdController: Option[ReplVcdController] = None

  def loadSource(input: String): Unit = {
    currentInterpreterOpt = Some(ExecutionEngine(input, optionsManager))
    currentInterpreterOpt.foreach { _ =>
      interpreter.setVerbose(interpreterOptions.setVerbose)
    }
    buildCompletions()
  }

  def loadFile(fileName: String): Unit = {
    var file = new File(fileName)
    if(! file.exists()) {
      file = new File(fileName + ".fir")
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    val input = io.Source.fromFile(file).mkString
    loadSource(input)
  }

  def loadScript(fileName: String): Unit = {
    currentScript = scriptFactory(fileName)
    currentScript match {
      case Some(script) =>
        console.println(s"loaded script file ${script.fileName} with ${script.length} lines")
      case _ =>
    }
  }

  def loadVcdScript(fileName: String): Unit = {
    val dutName = currentInterpreterOpt match {
      case Some(interpreter) => interpreter.ast.main
      case None => ""
    }
    try {
      currentVcdScript = Some(VCD.read(fileName, dutName))
      replVcdController = Some(new ReplVcdController(this, this.interpreter, currentVcdScript.get))
    }
    catch {
      case e: Exception =>
        console.println(s"Failed to load vcd script $fileName, error: ${e.getMessage}")
    }
  }

  def parseNumber(numberString: String): BigInt = {
    def parseWithRadix(numString: String, radix: Int): BigInt = {
      BigInt(numString, radix)
    }

    if(numberString.startsWith("0x"))     { parseWithRadix(numberString.drop(2), 16) }
    else if(numberString.startsWith("h")) { parseWithRadix(numberString.drop(1), 16) }
    else if(numberString.startsWith("o")) { parseWithRadix(numberString.drop(1), 8) }
    else if(numberString.startsWith("b")) { parseWithRadix(numberString.drop(1), 2) }
    else                                  { parseWithRadix(numberString, 10) }
  }
  // scalastyle:off number.of.methods
  object Commands {
    def getOneArg(failureMessage: String, argOption: Option[String] = None): Option[String] = {
      if(args.length == 2) {
        Some(args(1))
      }
      else if(args.length == 1 && argOption.isDefined) {
        Some(argOption.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    def getTwoArgs(failureMessage: String,
                   arg1Option: Option[String] = None,
                   arg2Option: Option[String] = None
                  ): (Option[String],Option[String]) = {
      if(args.length == 3) {
        (Some(args(1)), Some(args(2)))
      }
      else if(args.length == 2) {
        (Some(args(1)), arg2Option)
      }
      else if(args.length == 1) {
        (arg1Option, arg2Option)
      }
      else {
        error(failureMessage)
        (None, None)
      }
    }
    //noinspection ScalaStyle
    def getThreeArgs(failureMessage: String,
                     arg1Option: Option[String] = None,
                     arg2Option: Option[String] = None,
                     arg3Option: Option[String] = None
                  ): Option[(String,String,String)] = {
      (args.length, arg1Option, arg2Option, arg3Option) match {
        case (4, _, _, _)                             => Some(args(1), args(2), args(3))
        case (3, _, _, Some(arg3))                    => Some(args(1), args(2), arg3)
        case (2, _, Some(arg2), Some(arg3))           => Some(args(1), arg2, arg3)
        case (1, Some(arg1), Some(arg2), Some(arg3))  => Some(arg1, arg2, arg3)
        case _ =>
          error(failureMessage)
          None
      }
    }

    val commands: ArrayBuffer[Command] = ArrayBuffer.empty[Command]
    commands ++= Seq(
      new Command("load") {
        def usage: (String, String) = ("load fileName", "load/replace the current firrtl file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"load"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) => loadFile(fileName)
            case _ =>
          }
        }
      },
      new Command("script") {
        def usage: (String, String) = ("script fileName", "load a script from a text file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"script"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("script filename") match {
            case Some(fileName) => loadScript(fileName)

            case _ =>
          }
        }
      },
      new Command("run") {
        def usage: (String, String) = ("run [linesToRun|all|list|reset]", "run loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"run"}),
            new StringsCompleter(jlist(Seq("all", "reset", "list")))
          ))
        }
        def handleList(script: Script, listArg: Option[String]): Unit = {
          val (min, max) = listArg match {
            case Some(IntPattern(intString)) =>
              val windowHalfSize = intString.toInt
              (script.currentLine + 1 - windowHalfSize, script.currentLine + 2 + windowHalfSize)
            case Some(other) =>
              console.println(s"run list parameter=$other, parameter must be an positive integer")
              (0, 0)
            case _ =>
              (0, script.length)
          }
          console.println(
            script.lines.zipWithIndex.flatMap { case (line, index) =>
              if(index >= min && index < max) {
                if (index == script.currentLine + 1) {
                  Some(Console.GREEN + f"$index%3d $line" + Console.RESET)
                }
                else {
                  Some(f"$index%3d $line")
                }
              }
              else {
                None
              }
            }.mkString("\n")
          )
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getTwoArgs("run [lines|skip [n]|set n|all|reset|list [n]], default is 1 => run 1 line",
                arg1Option = Some("1"), arg2Option = None) match {
                case (Some("all"), _)   =>
                  console.println("run all")
                  if(script.atEnd) { script.reset() }
                  else { script.runRemaining() }
                case (Some("reset"), _) =>
                  script.reset()
                  handleList(script, Some("2"))
                case (Some("list"), listArg) =>
                  handleList(script, listArg)
                case (Some("skip"), listArg) =>
                  val skip = listArg match {
                    case Some(IntPattern(intString)) => intString.toInt
                    case _ => 1
                  }
                  script.setSkipLines(skip)
                case (Some("set"), listArg) =>
                  listArg match {
                    case Some(IntPattern(intString)) =>
                      script.setLine(intString.toInt)
                      handleList(script, Some("2"))
                    case _ =>
                      console.println("must specify set line number")
                  }
                case (Some(IntPattern(intString)), _) =>
                  val linesToRun = intString.toInt
                  script.setLinesToRun(linesToRun)
                case (None, None) =>
                  script.runRemaining()
                case (Some(arg), _) =>
                  error(s"unrecognized run_argument $arg")
              }
            case _ =>
              error(s"No current script")
          }
        }
        // scalastyle:on cyclomatic.complexity

      },
      new Command("vcd") {
        def usage: (String, String) = ("vcd [run|list|test|help]", "control vcd input file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"vcd"}),
            new ArgumentCompleter(
              new ArgumentCompleter(
                new StringsCompleter(jlist(Seq("run", "inputs", "list", "test")))
              ),
              new ArgumentCompleter(
                new StringsCompleter({"load"}),
                new FileNameCompleter
              )
            )
          ))
        }
        def run(args: Array[String]): Unit = {
          replVcdController match {
            case Some(controller) => controller.processListCommand(args)
            case _ => error(s"No current script")
          }
        }
      },
      new Command("record-vcd") {
        def usage: (String, String) = ("record-vcd [<fileName>]|[done]", "treadle.vcd loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"record-vcd"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("treadle.vcd [fileName|done]",
            argOption = Some("out.treadle.vcd")) match {
            case Some("done")   =>
              interpreter.disableVCD()
            case Some(fileName) =>
              interpreter.makeVCDLogger(
                fileName, showUnderscored = optionsManager.interpreterOptions.vcdShowUnderscored)
            case _ =>
              interpreter.disableVCD()
          }
        }
      },
      new Command("type") {
        private def peekableThings = interpreter.validNames.toSeq
        def usage: (String, String) = ("type regex", "show the current type of things matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "type"
              }),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("type regex") match {
            case Some((peekRegex)) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        val value = interpreter.getValue(settableThing)
                        console.println(s"type $settableThing $value")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry now settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("poke") {
        def usage: (String, String) = ("poke inputPortName value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "poke"
              }),
              new StringsCompleter(
                jlist(interpreter.getInputPorts ++ interpreter.getRegisterNames)
              )
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case (Some(portName), Some(valueString)) =>
              try {
                val numberValue = parseNumber(valueString)
                interpreter.setValue(portName, numberValue)
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("rpoke") {
        private def settableThings = {
          interpreter.getInputPorts ++ interpreter.getRegisterNames
        }
        def usage: (String, String) = ("rpoke regex value", "poke value into ports that match regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpoke"
              }),
              new StringsCompleter(jlist(settableThings))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("rpoke regex value") match {
            case (Some(pokeRegex), Some(valueString)) =>
              try {
                val pokeValue = parseNumber(valueString)
                val portRegex = pokeRegex.r
                val setThings = settableThings.flatMap { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      interpreter.setValue(settableThing, pokeValue)
                      Some(settableThing)
                    case _ => None
                  }
                }
                if(setThings.nonEmpty) {
                  console.println(s"poking value $pokeValue into ${setThings.toList.sorted.mkString(", ")}")
                }
                else {
                  console.println(s"Sorry now settable ports matched regex $pokeRegex")
                }


              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("peek") {
        def usage: (String, String) = ("peek componentName", "show the current value of the named circuit component")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "peek"
              }),
              new StringsCompleter(jlist(interpreter.validNames.toSeq))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("peek componentName") match {
            case Some(componentName) =>
              try {
                val value = interpreter.getValue(componentName)
                console.println(s"peek $componentName $value")
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage}")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("rpeek") {
        private def peekableThings = interpreter.validNames.toSeq
        def usage: (String, String) = ("rpeek regex", "show the current value of things matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpeek"
              }),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("rpeek regex") match {
            case Some((peekRegex)) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        val value = interpreter.getValue(settableThing)
                        console.println(s"rpeek $settableThing $value")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry now settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("randomize") {
        def usage: (String, String) = ("randomize", "randomize all inputs except reset)")
        def run(args: Array[String]): Unit = {
          for(symbol <- interpreter.symbols) {
            try {
              val newValue = makeRandom(symbol.firrtlType)
              interpreter.setValue(symbol.name, newValue)
              console.println(s"setting ${symbol.name} to $newValue")
            }
            catch {
              case e: Exception =>
                console.println(s"Error randomize: setting ${symbol.name}, error ${e.getMessage}")
            }
          }
//          for((component, value) <- engine.getRegisterNames) {
//            try {
//              val newValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = false)
//              engine.circuitState.registers(component) = newValue
//              val newNextValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = false)
//              engine.circuitState.nextRegisters(component) = newNextValue
//              console.println(s"setting $component to $newValue")
//            }
//            catch {
//              case e: Exception =>
//                console.println(s"Error randomize: setting $component to $value error ${e.getMessage}")
//            }
//          }
//          for(memory <- engine.circuitState.memories.values) {
//            for(memoryIndex <- 0 until memory.dataStore.length) {
//              memory.dataStore.update(
//                memoryIndex,
//                TypeInstanceFactory.makeRandomSimilar(memory.dataStore.underlyingData.head, poisoned = false))
//            }
//          }
          console.println(interpreter.getPrettyString)
        }
      },
//      new Command("poison") {
//        def usage: (String, String) = ("poison",
//          "poison everything)")
//        def run(args: Array[String]): Unit = {
//          for{
//            (component, value) <- engine.circuitState.inputPorts ++
//              engine.circuitState.outputPorts ++
//              engine.circuitState.ephemera
//          } {
//            engine.setValue(component, TypeInstanceFactory.makeRandomSimilar(value, poisoned = true))
//          }
//          for((component, value) <- engine.circuitState.registers) {
//            try {
//              val newValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = true)
//              engine.circuitState.registers(component) = newValue
//              val newNextValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = true)
//              engine.circuitState.nextRegisters(component) = newNextValue
//              console.println(s"setting $component to $newValue")
//            }
//            catch {
//              case e: Exception =>
//                console.println(s"Error poison: setting $component to $value error ${e.getMessage}")
//            }
//          }
//          for(memory <- engine.circuitState.memories.values) {
//            for(memoryIndex <- 0 until memory.dataStore.length) {
//              memory.dataStore.update(memoryIndex, TypeInstanceFactory(memory.dataType))
//            }
//          }
//          console.println(engine.circuitState.prettyString())
//        }
//      },
      new Command("reset") {
        def usage: (String, String) = ("reset [numberOfSteps]",
          "assert reset (if present) for numberOfSteps (default 1)")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "reset"
              })
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                interpreter.setValue("reset", 1)
                val numberOfSteps = numberOfStepsString.toInt
                for(_ <- 0 until numberOfSteps) {
                  interpreter.cycle()
                  interpreter.evaluateCircuit()
                }
                interpreter.setValue("reset", 0)
                // console.println(engine.circuitState.prettyString())
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("step") {
        def usage: (String, String) = ("step [numberOfSteps]",
          "cycle the clock numberOfSteps (default 1) times, and show state")
        def run(args: Array[String]): Unit = {
          getOneArg("step [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                val numberOfSteps = numberOfStepsString.toInt
                interpreter.timer("steps") {
                  for (_ <- 0 until numberOfSteps) {
                    interpreter.timer("step") {
                      interpreter.cycle()
                    }
                  }
                }
                if(! scriptRunning) {
                  // console.println(engine.circuitState.prettyString())
                  console.println(s"step $numberOfSteps in ${interpreter.timer.prettyLastTime("steps")}")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("waitfor") {
        def usage: (String, String) = ("waitfor componentName value [maxNumberOfSteps]",
          "wait for particular value (default 1) on component, up to maxNumberOfSteps (default 100)")
        def run(args: Array[String]): Unit = {
          getThreeArgs(
            "waitfor componentName [value] [maxNumberOfSteps]",
            arg2Option = Some("1"),
            arg3Option = Some("100")
          ) match {
            case Some((componentName, valueString, maxNumberOfStepsString)) =>
              try {
                val maxNumberOfSteps = maxNumberOfStepsString.toInt
                val value = valueString.toInt

                var tries = 0
                while(tries < maxNumberOfSteps && interpreter.getValue(componentName) != BigInt(value)) {
                  interpreter.cycle()
                  tries += 1
                }
                if(interpreter.getValue(componentName) != BigInt(value)) {
                  console.println(
                    s"waitfor exhausted $componentName did not take on value $value in $maxNumberOfSteps cycles")
                }
                else {
                  console.println(s"$componentName == value $value in $tries cycles")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("show") {
        def usage: (String, String) = ("show [state|input|lofirrtl]", "show useful things")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "show"}),
              new StringsCompleter(jlist(Seq("state", "input", "lofirrtl")))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("state")) match {
            case Some("lofirrtl") =>
              console.println(interpreter.ast.serialize)
            case Some("input") =>
              console.println(interpreter.ast.serialize)
            case _ =>
              console.println(interpreter.getPrettyString)
          }
        }
      },
      new Command("display") {
        def usage: (String, String) = ("how signal[, signal, ...]", "show computation of symbols")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "display"}),
              new StringsCompleter(jlist(interpreter.symbolTable.keys.toSeq))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("state")) match {
            case Some(symbolList) =>
              if(currentInterpreterOpt.isDefined) {
                console.println(interpreter.renderComputation(symbolList))
              }
            case _ =>
              console.println(interpreter.getPrettyString)
          }
        }
      },
      new Command("info") {
        def usage: (String, String) = ("info", "show information about the circuit")
        def run(args: Array[String]): Unit = {
          console.println(interpreter.getInfoString)
        }
      },
//      new Command("timing") {
//        def usage: (String, String) = ("timing [clear|bin]", "show the current timing state")
//        override def completer: Option[ArgumentCompleter] = {
//          if(currentInterpreterOpt.isEmpty) {
//            None
//          }
//          else {
//            Some(new ArgumentCompleter(
//              new StringsCompleter({ "timing"}),
//              new StringsCompleter(jlist(Seq("clear", "bin")))
//            ))
//          }
//        }
//        // scalastyle:off cyclomatic.complexity
//        def run(args: Array[String]): Unit = {
//          getOneArg("", Some("")) match {
//            case Some("clear") => engine.timer.clear()
//            case Some("bin") =>
//              val names = engine.dependencyGraph.validNames -- engine.dependencyGraph.inputPorts
//
//              val countPerName = new scala.collection.mutable.HashMap[Long, Long]
//              names.foreach { name =>
//                engine.timer.timingLog.get(name).foreach { t =>
//                  if(! countPerName.contains(t.events)) {
//                    countPerName(t.events) = 1
//                  }
//                  else {
//                    countPerName(t.events) = countPerName(t.events) + 1
//                  }
//                }
//              }
//              countPerName.keys.toSeq.sorted.foreach { count: Long =>
//                console.println(f"$count ${countPerName(count)}")
//              }
//            case _ =>
//              val names = engine.dependencyGraph.validNames -- engine.dependencyGraph.inputPorts
//
//              val sortedNames = names.toSeq.sortWith { case (a, b) =>
//                (engine.timer.timingLog.get(a), engine.timer.timingLog.get(b)) match {
//                  case (Some(t1), Some(t2)) =>
//                    if(t1.events == t2.events) {
//                      a < b
//                    }
//                    else {
//                      t1.events < t2.events
//                    }
//                  case (Some(_), None)      => false
//                  case (None, Some(_))      => true
//                  case _                    => a < b
//                }
//              }
//              for (name <- sortedNames) {
//                console.println(f"$name%-20s ${engine.timer.prettyEntryForTag(name)}")
//              }
//              console.println(f"${"Total"}%-20s ${engine.timer.prettyEntry(engine.timer.totalEvent)}")
//          }
//        }
//      },
      new Command("verbose") {
        def usage: (String, String) = ("verbose [true|false|toggle]",
          "set evaluator verbose mode (default toggle) during dependency evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "verbose"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("verbose must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") => interpreter.setVerbose(! interpreter.verbose)
            case Some("true")   => interpreter.setVerbose()
            case Some("false")  => interpreter.setVerbose(false)
            case _ =>
          }
          console.println(s"evaluator verbosity is now ${interpreter.verbose}")
        }
      },
      new Command("snapshot") {
        def usage: (String, String) = ("snapshot",
          "save state of engine")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "snapshot"}),
              new FileNameCompleter
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val writer = new PrintWriter(new File(fileName))
              writer.write(interpreter.dataStore.serialize)
              writer.close()
            case _ =>
          }
          console.println(interpreter.dataStore.serialize)
        }
      },
      new Command("restore") {
        def usage: (String, String) = ("restore",
          "save state of engine")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "restore"}),
              new FileNameCompleter
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val jsonSource = Source.fromFile(new File(fileName)).getLines().mkString("\n")
              interpreter.dataStore.deserialize(jsonSource)
            case _ =>
          }
        }
      },
//      new Command("allow-cycles") {
//        def usage: (String, String) = ("allow-cycles [true|false|toggle]",
//          "set evaluator allow combinational loops (could cause correctness problems")
//        override def completer: Option[ArgumentCompleter] = {
//          if(currentInterpreterOpt.isEmpty) {
//            None
//          }
//          else {
//            Some(new ArgumentCompleter(
//              new StringsCompleter({ "allow-cycles"}),
//              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
//            ))
//          }
//        }
//        def run(args: Array[String]): Unit = {
//          getOneArg("allow-cycles must be followed by true false or toggle", Some("toggle")) match {
//            case Some("toggle") =>
//              engine.evaluator.allowCombinationalLoops = ! engine.evaluator.allowCombinationalLoops
//            case Some("true")   => engine.evaluator.allowCombinationalLoops = true
//            case Some("false")  => engine.evaluator.allowCombinationalLoops = false
//            case _ =>
//          }
//          console.println(s"evaluator allow combinational loops is now ${engine.evaluator.evaluateAll}")
//        }
//      },
      new Command("help") {
        def usage: (String, String) = ("help", "show available commands")
        def run(args: Array[String]): Unit = {
          val maxColumn1Width = Commands.commands.map(_.usage._1.length).max + 2
          Commands.commands.foreach { command =>
            val (column1, column2) = command.usage
            terminal.getWidth

            console.println(s"$column1${" "*(maxColumn1Width - column1.length)} $column2")
          }
        }
      },
      new Command("quit") {
        def usage: (String, String) = ("quit", "exit the engine")
        def run(args: Array[String]): Unit = {
          if(! history.isEmpty) {
            history.removeLast()
          }
          done = true
        }
      }
    )
    val commandMap: Map[String, Command] = commands.map(command => command.name -> command).toMap
  }
  //scalastyle:on

  def buildCompletions(): Unit = {
    console.setCompletionHandler(new CandidateListCompletionHandler {})
    Commands.commands.flatMap { command =>
      command.completer
    }.foreach { completer =>
      console.addCompleter(completer)
    }
  }

  /**
    * gets the next line from either the current executing script or from the console.
    * Strips comments from the line, may result in empty string, command parser is ok with that
 *
    * @return
    */
  def getNextLine: String = {
    val rawLine = currentScript match {
      case Some(script) =>
        script.getNextLineOption match {
          case Some(line) =>
            console.println(s"[${script.currentLine}:${script.fileName}] $line")
            line
          case _ =>
            console.readLine()
        }
      case _ =>
        console.readLine()
    }
    if(rawLine == null) {
      history.add("quit")
      "quit"
    }
    else {
      rawLine.split("#").head
    }
  }

  def scriptRunning: Boolean = {
    currentScript match {
      case Some(script) => script.hasNext
      case _            => false
    }
  }

  //scalastyle:off method.length
  def run(): Unit = {
    if(replConfig.firrtlSource.nonEmpty) {
      loadSource(replConfig.firrtlSource)
    }
    else if(replConfig.firrtlSourceName.nonEmpty) {
      loadFile(replConfig.firrtlSourceName)
    }
    if(replConfig.scriptName.nonEmpty) {
      loadScript(replConfig.scriptName)
    }
    if(replConfig.useVcdScript) {
      loadVcdScript(optionsManager.getVcdFileName)
    }
    buildCompletions()

    console.setPrompt("firrtl>> ")

    if(replConfig.runScriptAtStart) {
      currentScript match {
        case Some(script) =>
          script.reset()
          script.runRemaining()
        case None =>
          console.println(s"Error: fr-run-script-at-startup set, with no script file")
      }
    }

    while (! done) {
      try {
        val line = getNextLine

        line.split(""";""").foreach { subLine =>

          args = subLine.trim.split(" +")

          if (args.length > 0) {
            if (Commands.commandMap.contains(args.head)) {
              Commands.commandMap(args.head).run(args.tail)
            }
            else {
              if (subLine.nonEmpty) error(s"unknown command $subLine, try help")
            }
          }
          else {
            error(s"unknown command: $subLine")
          }
        }
      }
      catch {
        case ie: TreadleException =>
          console.println(s"Interpreter Exception occurred: ${ie.getMessage}")
          ie.printStackTrace()
        case e: NullPointerException =>
          error(s"Null pointer exception, please file an issue\n ${e.getMessage}")
          e.printStackTrace()
        case e: Exception =>
          console.println(s"Exception occurred: ${e.getMessage}")
          e.printStackTrace()
      }
    }

    console.println(s"saving history ${history.size()}")
    console.flush()
    history.flush()
    console.shutdown()
    terminal.restore()
  }

  def error(message: String): Unit = {
    console.println(s"Error: $message")
  }

  def jlist(list: Seq[String]): java.util.List[String]= {
    val array = ArrayBuffer.empty[String]
    array ++= list
    array.asJava
  }
}

object TreadleRepl {
  def execute(optionsManager: InterpreterOptionsManager with HasReplConfig): Unit = {
    val repl = new TreadleRepl(optionsManager)
    repl.run()
  }

  def main(args: Array[String]): Unit = {
    val optionsManager = new InterpreterOptionsManager with HasReplConfig

    if(optionsManager.parse(args)) {
      Logger.makeScope(optionsManager) {
        val repl = new TreadleRepl(optionsManager)
        repl.run()
      }
    }
  }
}
