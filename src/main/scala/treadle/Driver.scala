// See LICENSE for license details.

package treadle

import firrtl.annotations.Annotation
import firrtl.{AnnotationSeq, ExecutionOptionsManager, HasFirrtlOptions}
import treadle.executable.{ClockInfo, TreadleException}

import scala.collection.mutable

//scalastyle:off magic.number
case class TreadleOptions(
                           writeVCD           : Boolean              = false,
                           vcdShowUnderscored : Boolean              = false,
                           setVerbose         : Boolean              = false,
                           setOrderedExec     : Boolean              = false,
                           allowCycles        : Boolean              = false,
                           randomSeed        : Long                 = System.currentTimeMillis(),
                           blackBoxFactories : Seq[ScalaBlackBoxFactory] = Seq.empty,
                           maxExecutionDepth : Long                 = Int.MaxValue,
                           showFirrtlAtLoad  : Boolean              = false,
                           lowCompileAtLoad  : Boolean              = true,
                           validIfIsRandom   : Boolean              = false,
                           rollbackBuffers   : Int                  = 0,
                           clockInfo         : Seq[ClockInfo]       = Seq.empty,
                           resetName         : String               = "reset",
                           callResetAtStartUp: Boolean              = false,
                           symbolsToWatch    : Seq[String]          = Seq.empty
  )
  extends firrtl.ComposableOptions {

  def vcdOutputFileName(optionsManager: ExecutionOptionsManager): String = {
    if(writeVCD) {
      s"${optionsManager.getBuildFileName("vcd")}"
    }
    else {
      ""
    }
  }

  def toAnnotations: AnnotationSeq = {
    var annotations = new mutable.ArrayBuffer[Annotation]

    if(writeVCD) { annotations += WriteVcdAnnotation }
    if(setVerbose) { annotations += VerboseAnnotation }
    if(allowCycles) { annotations += AllowCyclesAnnotation }
    if(showFirrtlAtLoad) { annotations += ShowFirrtlAtLoadAnnotation }
    if(validIfIsRandom) { annotations += ValidIfIsRandomAnnotation }
    if(callResetAtStartUp) { annotations += CallResetAtStartupAnnotation }
    if(! lowCompileAtLoad) { annotations += DontRunLoweringCompilerLoadAnnotation }

    if(rollbackBuffers != TreadleOptions().rollbackBuffers) {
      annotations += RollBackBuffersAnnotation(rollbackBuffers)
    }
    annotations += RandomSeedAnnotation(randomSeed)
    annotations += BlackBoxFactoriesAnnotation(blackBoxFactories)
    annotations += BlackBoxFactoriesAnnotation(blackBoxFactories)
    annotations += ClockInfoAnnotation(clockInfo)
    annotations += ResetNameAnnotation(resetName)
    annotations += SymbolsToWatchAnnotation(symbolsToWatch)

    AnnotationSeq(annotations.toSeq)
  }
}

trait HasTreadleOptions {
  self: ExecutionOptionsManager =>

  var treadleOptions = TreadleOptions()

  parser.note("treadle-options")

  parser.opt[Unit]("tr-write-vcd")
    .abbr("tiwv")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(writeVCD = true)
    }
    .text("writes vcd execution log, filename will be base on top")

  parser.opt[Unit]("tr-vcd-show-underscored-vars")
    .abbr("tivsuv")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(vcdShowUnderscored = true)
    }
    .text("vcd output by default does not show var that start with underscore, this overrides that")

  parser.opt[Unit]("tr-verbose")
    .abbr("tv")
    .foreach {_ =>
      treadleOptions = treadleOptions.copy(setVerbose = true)
    }
    .text("makes engine very verbose")

  parser.opt[Unit]("tr-ordered-exec")
    .abbr("tioe")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(setOrderedExec = true)
    }
    .text("operates on dependencies optimally, can increase overhead, makes verbose mode easier to read")

  parser.opt[Unit]("fr-allow-cycles")
    .abbr("tiac")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(allowCycles = true)
    }
    .text(s"allow combinational loops to be processed, though unreliable, default is ${treadleOptions.allowCycles}")

  parser.opt[Long]("tr-random-seed")
    .abbr("tirs")
      .valueName("<long-value>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(randomSeed = x)
    }
    .text("seed used for random numbers generated for tests and poison values, default is current time in ms")

  parser.opt[Unit]("show-firrtl-at-load")
    .abbr("tisfas")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(showFirrtlAtLoad = true)
    }
    .text("compiled low firrtl at firrtl load time")

  parser.opt[Unit]("dont-run-lower-compiler-on-load")
    .abbr("tilcol")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(lowCompileAtLoad = false)
    }
    .text("run lowering compiler when firrtl file is loaded")

  parser.opt[Unit]("validif-random")
    .abbr("tivir")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(validIfIsRandom = true)
    }
    .text("validIf returns random value when condition is false")

  parser.opt[Unit]("call-reset-at-start")
    .abbr("ticras")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(callResetAtStartUp = true)
    }
    .text("has the tester automatically do a reset on it's own at startup")

  parser.opt[Int]("tr-rollback-buffers")
    .abbr("tirb")
    .valueName("<int-value>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(rollbackBuffers = x)
    }
    .text("number of rollback buffers, 0 is no buffers, default is 4")

  def parseClockInfo(input: String): ClockInfo = {
    input.split(":").map(_.trim).toList match {
      case name :: Nil =>
        ClockInfo(name)
      case name :: period :: Nil =>
        ClockInfo(name, period.toLong)
      case name :: period :: offset :: Nil =>
        ClockInfo(name, period.toLong, offset.toLong)
      case _ =>
        throw TreadleException(s"Bad clock info string $input, should be name[:period[:offset]]")
    }
  }
  parser.opt[String]("tr-clock-info")
    .abbr("tici")
    .unbounded()
    .valueName("<string>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(clockInfo = treadleOptions.clockInfo ++ Seq(parseClockInfo(x)))
    }
    .text("clock-name[:period[:initial-offset]]")

  parser.opt[Seq[String]]("tr-symbols-to-watch")
    .abbr("tstw")
    .valueName("symbols]")
    .foreach { x =>
    treadleOptions = treadleOptions.copy(symbolsToWatch = x)
    }
    .text("symbol[,symbol[...]")

  parser.opt[String]("tr-reset-name")
    .abbr("tirn")
    .valueName("<string>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(resetName = x)
    }
    .text("name of default reset")
}

object Driver {

  def execute(firrtlInput: String, optionsManager: TreadleOptionsManager): Option[TreadleTester] = {
    val tester = TreadleTester(firrtlInput, optionsManager)
    Some(tester)
  }

  def execute(args: Array[String], firrtlInput: String): Option[TreadleTester] = {
    val optionsManager = new TreadleOptionsManager

    if (optionsManager.parser.parse(args)) {
      execute(firrtlInput, optionsManager)
    } else {
      None
    }
  }
}

class TreadleOptionsManager extends ExecutionOptionsManager("engine") with HasTreadleSuite

trait HasTreadleSuite extends ExecutionOptionsManager with HasFirrtlOptions with HasTreadleOptions {
  self : ExecutionOptionsManager =>

  def toAnnotationSeq: AnnotationSeq = {
    commonOptions.toAnnotations ++ firrtlOptions.toAnnotations ++ treadleOptions.toAnnotations
  }
}
