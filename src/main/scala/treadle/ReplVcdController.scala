// See LICENSE for license details.

package treadle

import treadle.executable.{Scheduler, SymbolTable}
import treadle.vcd.{Change, VCD, Wire}

import scala.tools.jline.console.ConsoleReader
import scala.util.matching.Regex

class ReplVcdController(val repl: TreadleRepl, val interpreter: ExecutionEngine, val vcd: VCD) {
  val console: ConsoleReader = repl.console

  // The following three elements track state of running the vcd file
  var currentTime: Long = 0L
  var currentTimeIndex: Int = 0
  val timeStamps: Array[Long] = vcd.valuesAtTime.keys.toList.sorted.toArray

  // The following control the current list state of the vcd file
  var currentListLocation: Int = 0
  var currentListSize: Int = 10

  var testAfterRun: Boolean = true
  var runVerbose: Boolean = true

  val IntPattern: Regex = """(-?\d+)""".r

  val vcdCircuitState: Scheduler = interpreter.scheduler

  val inputs: Set[String] = {
    vcd.scopeRoot.wires
      .filter { wire =>
        interpreter.isInputPort(wire.name)
      }
      .map(_.name).toSet
  }

  val outputs: Set[Wire] = {
    vcd.scopeRoot.wires.filter { wire =>
      interpreter.isOutputPort(wire.name)
    }.toSet
  }

  def showInputMap(): Unit = {
    vcd.scopeRoot.wires.foreach { wire =>
      console.println(s"vcd top level wire $wire")
    }
  }

  def now: String = {
    showEvent(currentTimeIndex)
  }

  def showEvent(timeIndex: Int): String = {
    s"Event: $timeIndex Time: ${timeStamps(timeIndex)}"
  }

  def showInputs(timeIndex: Int): Unit = {
    var hasStep = false
    if(timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(now)
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      if(inputs.contains(change.wire.name)) {
        if(change.wire.name == "clock" && change.value == BigInt(0)) {
          hasStep = true
        }
        else {
          console.println(s"poke ${change.wire.name} ${change.value}")
        }
      }
    }
    if(hasStep) {
      console.println(s"step 1")
    }
    if(timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  def showChanges(timeIndex: Int): Unit = {
    var hasStep = false
    if(timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(showEvent(timeIndex))
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      if(change.wire.name == "clock" && change.value == BigInt(0)) {
        hasStep = true
      }
      else if(inputs.contains(change.wire.name)) {
        console.println(s"poke ${change.wire.fullName} ${change.value}")
      }
      else {
        console.println(s"changed: ${change.wire.fullName} to ${change.value}")
      }
    }
    if(hasStep) {
      console.println(s"step 1")
    }
    if(timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  //TODO (chick) needToStep used to check for clock not found, should probably check this more
  def stepOnPosEdgelock(): Boolean = {
    var needToStep = false
    vcd
      .valuesAtTime(timeStamps(currentTimeIndex))
      .find { change => change.wire.fullName == "clock"}
      .foreach { clock: Change =>
        needToStep =
          interpreter.getValue(clock.wire.fullName) == BigInt(0) &&
          clock.value == BigInt(1)
      }

    if(needToStep) {
      console.println(s"vcd step called at $now")
      repl.step()
    }
    needToStep
  }

  //scalastyle:off method.length
  /**
    * Applies changes to circuit based on current vcd time step to current inputs.
    *
    * @note At time step zero all possible changes are applied.
    * @return
    */
  def doChanges(): Boolean = {
    def showProgress(message: => String): Unit = {
      if(runVerbose) console.println(message)
    }
    val stepped = stepOnPosEdgelock()

    vcd.valuesAtTime(timeStamps(currentTimeIndex)).foreach { change =>
      //      val name = change.wire.name
      val fullName = change.wire.fullName
      val newValue = change.value

      val wireId = change.wire.id

      updateCircuitState(fullName, change.wire)

      if(vcd.aliasedWires.contains(wireId)) {
        vcd.aliasedWires(wireId).foreach { aliasedWire =>
          updateCircuitState(aliasedWire.fullName, aliasedWire, s" -- shared with $fullName")
        }
      }

      def updateCircuitState(fullName: String, wire: Wire, message: String = ""): Unit = {
        if (inputs.contains(fullName)) {
          showProgress(s"poke $fullName $newValue $message")

          interpreter.setValue(fullName, newValue)
          interpreter.setValue(fullName, interpreter.getValue(fullName))
        }
        else if(interpreter.symbolTable.contains(fullName)) {
          val isRegister = interpreter.isRegister(fullName)
          val name = if(isRegister) {
            SymbolTable.makeRegisterInputName(fullName)
          }
          else {
            fullName
          }

          if(currentTimeIndex == 0) {
            /* if first time increment populate components other than inputs */
            interpreter.setValue(name, newValue, registerPoke = isRegister)
          }
          interpreter.setValue(name, newValue, registerPoke = isRegister)
          showProgress(s"recording: $name to $newValue $message")
        }
        else {
          // showProgress(s"Don't know how to process entry: change $fullName to $newValue")
        }
      }
    }
    stepped
  }

  def hasStep(timeIndex: Int): Boolean = {
    if(currentTimeIndex < timeStamps.length) {
      vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
        if(inputs.contains(change.wire.name)) {
          if(change.wire.name == "clock" && change.value == BigInt(0)) {
            return true
          }
        }
      }
    }
    false
  }

  def runUsage: String = {
    """vcd run                    run one event
      |vcd run to step            run event until a step occurs
      |vcd run to <event-number>  run up to given event-number
      |vcd run <number-of-events> run this many events
      |vcd run set <event>        set next event to run
      |vcd run test               test outputs after each run command
      |vcd run notest             do not test outputs after each run command
      |vcd run verbose            run in verbose mode (the default)
      |vcd run noverbose          do not run in verbose mode
      |""".stripMargin
  }

  //scalastyle:off cyclomatic.complexity method.length
  def run(parameters: Array[String]): Unit = {
    parameters.toList match {
      case Nil =>
        if(currentTimeIndex < timeStamps.length) {
          doChanges()
          if(testAfterRun) checkCurrentValueOfOutputs()
          currentTimeIndex += 1
        }
      case "to" :: tail => tail match {
        case IntPattern(nString) :: _ =>
          val n = nString.toInt
          if(n <= currentTimeIndex) {
            console.println(s"run to $n, error, $n must be greater then current time index ${currentTimeIndex + 1}")
          }
          else {
            while (currentTimeIndex <= n & currentTimeIndex < timeStamps.length) {
              doChanges()
              currentTimeIndex += 1
            }
            if(testAfterRun) checkCurrentValueOfOutputs()
          }
        case "step" :: _ =>
          while(currentTimeIndex < timeStamps.length && !doChanges()) {
            // repeat until no more events or doChange returns false when step has occurred
            currentTimeIndex += 1
          }
          if(testAfterRun) checkCurrentValueOfOutputs()
      }
      case "test" :: _ =>
        testAfterRun = true
      case "notest" :: _ =>
        testAfterRun = false
      case "verbose" :: _ =>
        runVerbose = true
      case "noverbose" :: _ =>
        runVerbose = false
      case "all" :: _ =>
        while(currentTimeIndex < timeStamps.length) {
          doChanges()
          currentTimeIndex += 1
        }
      case arg :: Nil =>
        arg match {
          case IntPattern(nString) =>
            for {
              _ <- 0 until nString.toInt
              if currentTimeIndex < timeStamps.length
            } {
              doChanges()
              currentTimeIndex += 1
            }
            if(testAfterRun) checkCurrentValueOfOutputs()
          case _ =>
            console.println(s"Unknown run command ${parameters.mkString(" ")}")
            console.println(runUsage)
        }
      case "set" :: tail => tail match {
        case IntPattern(nString) :: _ =>
          currentTimeIndex = nString.toInt
        case _ =>
          console.println(s"vcd next set requires event number")
      }
      case _ =>
        console.println(s"Unknown next command ${parameters.mkString(" ")}")
        console.println(runUsage)
    }
  }
  //scalastyle:on cyclomatic.complexity

  //TODO: (chick) this function does nothing right now
  def checkCurrentValueOfOutputs(): Unit = {
//    if (currentTimeIndex >= 0 && currentTimeIndex < timeStamps.length) {
//      console.println(s"Testing outputs $now ${"=" * 20}")
//      def show(mismatch: Boolean, message: String): Unit = {
//        val prefix = if (mismatch) Console.RED else ""
//        val suffix = if (mismatch) Console.RESET else ""
//        console.println(prefix + message + suffix)
//      }
//
//    }
  }

  def test(parameters: Array[String]): Unit = {
    parameters.toList match {
      case "outputs" :: _ =>
        if(currentTimeIndex > 0) {
          checkCurrentValueOfOutputs()
        }

      case _ =>
        console.println(s"Unknown test command ${parameters.mkString(" ")}")
    }
  }

  def show(lo: Int, hi: Int): Unit = {
    for(timeIndex <- lo until hi) {
      if(timeIndex < timeStamps.length) {
        showChanges(timeIndex)
      }
    }
  }

  def showCurrent(): Unit = {
    val (lo, hi) = (0.max(currentListLocation), timeStamps.length.min(currentListLocation + currentListSize))
    show(lo, hi)
    currentListLocation += currentListSize
  }

  def listUsage: String = {
    """vcd list
      |vcd list all
      |vcd list <event-number>
      |vcd list <event-number> <window-size>
    """.stripMargin
  }

  def list(parameters: Array[String]): Unit = {
    parameters.toList match {
      case Nil =>
        showCurrent()
      case "all" :: _ =>
        for(_ <- timeStamps.indices) {
          show(0, timeStamps.length)
        }
        currentListLocation = currentTimeIndex + 1
      case IntPattern(nString) :: IntPattern(eventString) :: _ =>
        currentListLocation = nString.toInt - 1
        currentListSize = eventString.toInt
        showCurrent()
      case IntPattern(nString) :: _ =>
        currentListLocation = nString.toInt - 1
        showCurrent()
      case _ =>
        console.println(s"Unknown list command list ${parameters.mkString(" ")} should be more like")
        console.println(listUsage)
    }
  }

  def usage: String = {
    runUsage + listUsage
  }

  def loadVcd(parameters: Array[String]): Unit = {
    parameters.toList match {
      case fileName :: _ =>
        repl.loadVcdScript(fileName)
      case Nil =>
        if(repl.optionsManager.getVcdFileName.nonEmpty) {
          repl.loadVcdScript(repl.optionsManager.getVcdFileName)
        }
    }
  }

  def infoUsage: String = {
    s"""
       |vcd info
     """.stripMargin
  }

  /**
    * command parser for vcd family of repl commands
    *
    * @param args arguments from user
    */
  def processListCommand(args: Array[String]): Unit = {
    args.headOption match {
      case Some("load") =>
        loadVcd(args.tail)
      case Some("inputs") =>
        showInputMap()
      case Some("run") =>
        run(args.tail)
      case Some("list") =>
        list(args.tail)
      case Some("info") =>
        console.println(vcd.info)
        console.println(f"run event:      $currentTimeIndex%8d")
        console.println(f"list position:  $currentListLocation%8d")
        console.println(f"list size:      $currentListSize%8d")
      case Some("test") =>
        test(args.tail)
      case Some("help") =>
        console.println(usage)
      case _ =>
        console.println(usage)
    }
  }
}
