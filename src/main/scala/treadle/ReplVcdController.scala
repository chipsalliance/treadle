// See LICENSE for license details.

package treadle

import treadle.vcd.{VCD, Wire}

import scala.tools.jline.console.ConsoleReader
import scala.util.matching.Regex

class ReplVcdController(val repl: TreadleRepl, val engine: ExecutionEngine, val vcd: VCD) {
  val console: ConsoleReader = repl.console

  // The following three elements track state of running the vcd file
  var currentTimeIndex: Int = 0
  val timeStamps: Array[Long] = vcd.valuesAtTime.keys.toList.sorted.toArray

  // The following control the current list state of the vcd file
  var currentListLocation: Int = 0
  var currentListSize: Int = 10

  // The following control the current execution options
  var testAfterRun : Boolean = true
  var justSetInputs: Boolean = true

  val IntPattern: Regex = """(-?\d+)""".r

  val inputs: Set[String] = {
    vcd.scopeRoot.wires
      .filter { wire =>
        engine.isInputPort(wire.name)
      }
      .map(_.name).toSet
  }

  val vcdRunner: VcdRunner = new VcdRunner(repl.currentTreadleTester, vcd)

  val outputs: Set[Wire] = {
    vcd.scopeRoot.wires.filter { wire =>
      engine.isOutputPort(wire.name)
    }.toSet
  }

  def now: String = {
    showEvent(currentTimeIndex)
  }

  def showEvent(timeIndex: Int): String = {
    vcdRunner.eventSummary(timeIndex)
  }

  def showInputs(timeIndex: Int): Unit = {
    var hasStep = false
    if(timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(now)
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      if(inputs.contains(change.wire.name)) {
        console.println(s"       ${change.wire.name} <= ${change.value}")
      }
    }
    if(timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  def showChanges(timeIndex: Int, showDetail: Boolean = false): Unit = {
    if(timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(showEvent(timeIndex))
    if(showDetail) {
      vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
        console.println(s"        ${change.wire.fullName} <= ${change.value}")
      }
    }
    if(timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  //scalastyle:off method.length
  /**
    * Applies changes to circuit based on current vcd time step to current inputs.
    *
    * @note At time step zero all possible changes are applied.
    * @return
    */
  def doChanges(): Unit = {
    vcdRunner.executeNextEvent()
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
      |vcd run all                run all remaining
      |vcd run to step            run event until a step occurs
      |vcd run to <event-number>  run up to given event-number
      |vcd run <number-of-events> run this many events, from current
      |vcd run set <event>        set next event to run
      |vcd run test               set run to test outputs as events are processed
      |vcd run notest             turn off testing of outputs as events are processed
      |vcd run justSetInputs      just set the inputs from the vcd script
      |vcd run setAllWires        set all wires from vcd script
      |vcd run verbose            run in verbose mode (the default)
      |vcd run noverbose          do not run in verbose mode
      |""".stripMargin
  }

  //scalastyle:off cyclomatic.complexity method.length
  def run(parameters: Array[String]): Unit = {
    parameters.toList match {
      case Nil =>
        vcdRunner.executeNextEvent()

      case "to" :: tail => tail match {
        case IntPattern(nString) :: _ =>
          val n = nString.toInt
          if(n <= currentTimeIndex) {
            console.println(s"run to $n, error, $n must be greater then current time index ${currentTimeIndex + 1}")
          }
          else {
            while (vcdRunner.nextEvent <= n & currentTimeIndex < timeStamps.length) {
              vcdRunner.executeNextEvent()
            }
            if(testAfterRun) checkCurrentValueOfOutputs()
          }
        case "step" :: _ =>
          var upClockFound = false
          while(vcdRunner.hasNextEvent && !upClockFound) {
            upClockFound = vcdRunner.nextEventHasClockUp
            vcdRunner.executeNextEvent()
          }
          if(testAfterRun) checkCurrentValueOfOutputs()
      }
      case "test" :: _ =>
        testAfterRun = true
      case "notest" :: _ =>
        testAfterRun = false
      case "justSetInputs" :: _ =>
        vcdRunner.justSetInputs = true
      case "setAllWires" :: _ =>
        vcdRunner.justSetInputs = false
      case "verbose" :: _ =>
        vcdRunner.verbose = true
      case "noverbose" :: _ =>
        vcdRunner.verbose = false
      case "all" :: _ =>
        while(vcdRunner.hasNextEvent) {
          vcdRunner.executeNextEvent()
        }
      case arg :: Nil =>
        arg match {
          case IntPattern(nString) =>
            for(_ <- 0 until nString.toInt.max(vcdRunner.events.length)) {
              vcdRunner.executeNextEvent()
            }
            if(testAfterRun) {
              vcdRunner.testWires(vcdRunner.previousEvent)
            }
          case _ =>
            console.println(s"Unknown run command ${parameters.mkString(" ")}")
            console.println(runUsage)
        }
      case "set" :: tail => tail match {
        case IntPattern(nString) :: _ =>
          vcdRunner.setNextEvent(nString.toInt)
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
        showChanges(timeIndex, showDetail = lo == hi - 1)
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
        show(lo = 0, hi = timeStamps.length)
        currentListLocation = currentTimeIndex + 1
      case IntPattern(nString) :: IntPattern(eventString) :: _ =>
        currentListLocation = nString.toInt
        currentListSize = eventString.toInt
        showCurrent()
      case IntPattern(nString) :: _ =>
        currentListLocation = nString.toInt
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
        showInputs(currentTimeIndex)
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
