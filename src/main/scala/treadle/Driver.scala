// See LICENSE for license details.

package treadle

import firrtl.{ExecutionOptionsManager, HasFirrtlExecutionOptions, FirrtlSourceAnnotation}
import firrtl.annotations.{Annotation, NoTargetAnnotation, SingleStringAnnotation}
import treadle.executable.ClockInfo

sealed trait TreadleOption { self: Annotation => }
case object WriteVcdAnnotation extends NoTargetAnnotation with TreadleOption
case object VcdShowUnderscoredAnnotation extends NoTargetAnnotation with TreadleOption
case object VerboseAnnotation extends NoTargetAnnotation with TreadleOption
case object SetOrderedExecAnnotation extends NoTargetAnnotation with TreadleOption
case object AllowCyclesAnnotation extends NoTargetAnnotation with TreadleOption
case class RandomSeedAnnotation(value: Long) extends NoTargetAnnotation with TreadleOption
case class BlackBoxFactoryAnnotation(value: BlackBoxFactory) extends NoTargetAnnotation with TreadleOption
case class MaxExecutionDepthAnnotation(value: Long) extends NoTargetAnnotation with TreadleOption
case object ShowFirrtlAtLoadAnnotation extends NoTargetAnnotation with TreadleOption
case object LowCompileAtLoadAnnotation extends NoTargetAnnotation with TreadleOption
case object ValidIfIsRandomAnnotation extends NoTargetAnnotation with TreadleOption
case class RollbackBuffersAnnotation(value: Int) extends NoTargetAnnotation with TreadleOption
case class ClockInfoAnnotation(value: ClockInfo) extends NoTargetAnnotation with TreadleOption
case class ResetNameAnnotation(value: String) extends SingleStringAnnotation with TreadleOption
case object NoDefaultResetAnnotation extends NoTargetAnnotation with TreadleOption
case class SymbolToWatchAnnotation(value: String) extends SingleStringAnnotation with TreadleOption

//scalastyle:off magic.number
case class TreadleOptions(
  writeVCD           : Boolean              = false,
  vcdShowUnderscored : Boolean              = false,
  setVerbose         : Boolean              = false,
  setOrderedExec     : Boolean              = false,
  allowCycles        : Boolean              = false,
  randomSeed         : Long                 = System.currentTimeMillis(),
  blackBoxFactories  : Seq[BlackBoxFactory] = Seq.empty,
  maxExecutionDepth  : Long                 = Int.MaxValue,
  showFirrtlAtLoad   : Boolean              = false,
  lowCompileAtLoad   : Boolean              = true,
  validIfIsRandom    : Boolean              = false,
  rollbackBuffers    : Int                  = 4,
  clockInfo          : Seq[ClockInfo]       = Seq.empty,
  resetName          : String               = "reset",
  noDefaultReset     : Boolean              = false,
  symbolsToWatch     : Seq[String]          = Seq.empty
) {

  def vcdOutputFileName(optionsManager: ExecutionOptionsManager with HasFirrtlExecutionOptions): String = {
    if(writeVCD) {
      s"${optionsManager.getBuildFileName("vcd")}"
    }
    else {
      ""
    }
  }
}

trait HasTreadleOptions {
  self: ExecutionOptionsManager =>

  lazy val treadleOptions: TreadleOptions = options
    .collect{ case t: TreadleOption => t }
    .foldLeft(TreadleOptions())( (t, x) =>
      x match {
        case WriteVcdAnnotation             => t.copy(writeVCD = true)
        case VcdShowUnderscoredAnnotation   => t.copy(vcdShowUnderscored = true)
        case VerboseAnnotation              => t.copy(setVerbose = true)
        case SetOrderedExecAnnotation       => t.copy(setOrderedExec = true)
        case AllowCyclesAnnotation          => t.copy(allowCycles = true)
        case RandomSeedAnnotation(s)        => t.copy(randomSeed = s)
        case BlackBoxFactoryAnnotation(b)   => t.copy(blackBoxFactories = t.blackBoxFactories :+ b)
        case MaxExecutionDepthAnnotation(d) => t.copy(maxExecutionDepth = d)
        case ShowFirrtlAtLoadAnnotation     => t.copy(showFirrtlAtLoad = true)
        case LowCompileAtLoadAnnotation     => t.copy(lowCompileAtLoad = false)
        case ValidIfIsRandomAnnotation      => t.copy(validIfIsRandom = true)
        case RollbackBuffersAnnotation(r)   => t.copy(rollbackBuffers = r)
        case ClockInfoAnnotation(i)         => t.copy(clockInfo = t.clockInfo :+ i)
        case ResetNameAnnotation(n)         => t.copy(resetName = n)
        case NoDefaultResetAnnotation       => t.copy(noDefaultReset = true)
        case SymbolToWatchAnnotation(s)     => t.copy(symbolsToWatch = t.symbolsToWatch :+ s)
      }
    )

  parser.note("firrtl-engine-options")

  parser.opt[Unit]("fint-write-vcd")
    .abbr("fiwv")
    .action( (_, a) => a :+ WriteVcdAnnotation )
    .text("writes vcd execution log, filename will be base on top")

  parser.opt[Unit]("fint-vcd-show-underscored-vars")
    .abbr("fivsuv")
    .action( (_, a) => a :+ VcdShowUnderscoredAnnotation )
    .text("vcd output by default does not show var that start with underscore, this overrides that")

  parser.opt[Unit]("fint-verbose")
    .abbr("fiv")
    .action( (_, a) => a :+ VerboseAnnotation )
    .text("makes engine very verbose")

  parser.opt[Unit]("fint-ordered-exec")
    .abbr("fioe")
    .action( (_, a) => a :+ SetOrderedExecAnnotation )
    .text("operates on dependencies optimally, can increase overhead, makes verbose mode easier to read")

  parser.opt[Unit]("fr-allow-cycles")
    .abbr("fiac")
    .action( (_, a) => a :+ AllowCyclesAnnotation )
    .text(s"allow combinational loops to be processed, though unreliable, default is ${new TreadleOptions().allowCycles}")

  parser.opt[Long]("fint-random-seed")
    .abbr("firs")
    .valueName("<long-value>")
    .action( (x, a) => a :+ RandomSeedAnnotation(x) )
    .text("seed used for random numbers generated for tests and poison values, default is current time in ms")

  parser.opt[String]("blackbox-factory")
    .action( (x, a) => try {
              a :+ BlackBoxFactoryAnnotation(Class.forName(x).asInstanceOf[Class[_<:BlackBoxFactory]].newInstance())
            } catch {
              case e: ClassNotFoundException => throw TreadleException(s"Unable to find class '$x' (did you misspell it?)")
            })
    .text("A class (with full path) that provides black box implementations")

  parser.opt[Long]("max-execution-depth")
    .action( (x, a) => a :+ MaxExecutionDepthAnnotation(x) )
    .text(s"maximum execution depth (default: ${new TreadleOptions().maxExecutionDepth})")

  parser.opt[Unit]("show-firrtl-at-load")
    .abbr("fisfas")
    .action( (_, a) => a :+ ShowFirrtlAtLoadAnnotation )
    .text("compiled low firrtl at firrtl load time")

  parser.opt[Unit]("dont-run-lower-compiler-on-load")
    .abbr("filcol")
    .action( (_, a) => a :+ LowCompileAtLoadAnnotation )
    .text("run lowering compiler when firrtl file is loaded")

  parser.opt[Unit]("validif-random")
    .abbr("fivir")
    .action( (_, a) => a :+ ValidIfIsRandomAnnotation )
    .text("validIf returns random value when condition is false")

  parser.opt[Int]("fint-rollback-buffers")
    .abbr("firb")
    .valueName("<int-value>")
    .action( (x, a) => a :+ RollbackBuffersAnnotation(x) )
    .text(s"number of rollback buffers, 0 is no buffers, (default: ${new TreadleOptions().rollbackBuffers})")

  def parseClockInfo(input: String): ClockInfo = {
    input.split(":").map(_.trim).toList match {
      case name :: Nil =>
        ClockInfo(name)
      case name :: period :: Nil =>
        ClockInfo(name, period.toLong)
      case name :: period :: offset :: Nil =>
        ClockInfo(name, period.toLong, offset.toLong)
      case _ =>
        throw new TreadleException(s"Bad clock info string $input, should be name[:period[:offset]]")
    }
  }
  parser.opt[String]("fint-clock-info")
    .abbr("fici")
    .unbounded()
    .valueName("<string>")
    .action( (x, a) => a :+ ClockInfoAnnotation(parseClockInfo(x)) )
    .text("clock-name[:period[:initial-offset]]")

  parser.opt[String]("fint-reset-name")
    .abbr("firn")
    .valueName("<string>")
    .action( (x, a) => a :+ ResetNameAnnotation(x) )
    .text("name of default reset")

  parser.opt[Unit]("no-default-reset")
    .abbr("findr")
    .action( (_, a) => a :+ NoDefaultResetAnnotation )
    .text("this prevents the tester from doing reset on it's own at startup")

  parser.opt[Seq[String]]("symbols-to-watch")
    .action( (x, a) => a ++ x.map(SymbolToWatchAnnotation(_)) )
    .text("Symbols in the circuit to watch")
}

object Driver {

  @deprecated("Use Driver.execute(optionsManager: HasTreadleSuite)", "1.0")
  def execute(firrtlInput: String, optionsManager: TreadleOptionsManager): Option[TreadleTester] = {
    val optionsManagerx = new ExecutionOptionsManager(
      applicationName=optionsManager.applicationName,
      args=Array.empty,
      annotations=optionsManager.options :+ FirrtlSourceAnnotation(firrtlInput) ) with HasTreadleSuite
    execute(optionsManagerx)
  }

  def execute(optionsManager: HasTreadleSuite): Option[TreadleTester] =
    Some(new TreadleTester(optionsManager))

  def execute(args: Array[String], firrtlInput: String): Option[TreadleTester] = {
    val opts = new ExecutionOptionsManager(
      applicationName="engine",
      args=args,
      annotations=Seq(FirrtlSourceAnnotation(firrtlInput))) with HasTreadleSuite
    execute(opts)
  }
}

class TreadleOptionsManager(args: Array[String]) extends ExecutionOptionsManager("engine", args) with HasTreadleSuite

trait HasTreadleSuite extends ExecutionOptionsManager with HasFirrtlExecutionOptions with HasTreadleOptions {
  self : ExecutionOptionsManager =>
}
