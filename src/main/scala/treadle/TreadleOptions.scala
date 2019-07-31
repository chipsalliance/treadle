// See LICENSE for license details.

package treadle

import firrtl.CircuitState
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.ir.Circuit
import firrtl.options.{HasShellOptions, RegisteredLibrary, ShellOption, Unserializable}
import firrtl.stage.{FirrtlFileAnnotation, FirrtlSourceAnnotation}
import treadle.executable.{ClockInfo, TreadleException}

sealed trait TreadleOption extends Unserializable { this: Annotation => }

/**
  * Tells treadle to write a vcd file during simulation
  */
case object WriteVcdAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd executioin log, filename will be based on top-name"
    )
  )
}

/**
  * Tells treadle to include _T_* and _GEN_* wires in VCD output
  */
case object VcdShowUnderScoredAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-vcd-show-underscored-vars",
      toAnnotationSeq = _ => Seq(VcdShowUnderScoredAnnotation),
      helpText = "vcd output by default does not show var that start with underscore, this overrides that"
    )
  )
}

/**
  *  Tells treadle to execute verbosely
  */
case object VerboseAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-verbose",
      toAnnotationSeq = _ => Seq(VerboseAnnotation),
      helpText = "makes the treadle very verbose"
    )
  )
}

/**
  *  Tells treadle to allow cycles
  */
case object AllowCyclesAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-allow-cycle",
      toAnnotationSeq = _ => Seq(AllowCyclesAnnotation),
      helpText = "will try to run when firrtl contains combinational loops"
    )
  )
}

/**
  *  Sets the seed for treadle's private random number generator
  */
case class RandomSeedAnnotation(seed: Long = 0L) extends NoTargetAnnotation with TreadleOption

object RandomSeedAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Long](
      longOption = "tr-random-seed",
      toAnnotationSeq = (seed: Long) => Seq(RandomSeedAnnotation(seed)),
      helpText = "sets the seed for Treadle's random number generator"
    )
  )
}

/**
  *  Tells treadle to show the low firrtl it is starting out with
  */
case object ShowFirrtlAtLoadAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-show-firrtl-at-load",
      toAnnotationSeq = _ => Seq(),
      helpText = "show the low firrtl source treadle is using to build simulator"
    )
  )
}

/**
  *  Tells treadle to not run its own lowering pass on firrtl input (not recommended)
  */
case object DontRunLoweringCompilerLoadAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-dont-run-lower-compiler-on-load",
      toAnnotationSeq = _ => Seq(),
      helpText = "do not run its own lowering pass on firrtl input (not recommended)"
    )
  )
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object ValidIfIsRandomAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-validif-random",
      toAnnotationSeq = _ => Seq(ValidIfIsRandomAnnotation),
      helpText = "validIf returns random value when condition is false"
    )
  )
}

/**
  *  Sets the number of rollback buffers in simulator, useful to see why wires have their values
  */
case class RollBackBuffersAnnotation(rollbackBufferDepth: Int = TreadleDefaults.RollbackBuffers)
        extends NoTargetAnnotation with TreadleOption

case object RollBackBuffersAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Int](
      longOption = "tr-rollback-buffers",
      toAnnotationSeq = (buffers: Int) => Seq(RollBackBuffersAnnotation(buffers)),
      helpText = s"number of rollback buffers, 0 is no buffers, default is ${TreadleDefaults.RollbackBuffers}"
    )
  )
}

/**
  *  Sets one or more clocks including their frequencies and phase
  */
case class ClockInfoAnnotation(clockInfoSeq: Seq[ClockInfo] = Seq(ClockInfo()))
        extends NoTargetAnnotation with TreadleOption

case object ClockInfoAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "tr-clock-info",
      toAnnotationSeq = (s: Seq[String]) => Seq(ClockInfoAnnotation(s.map(parseClockInfo))),
      helpText = "comma separated list of clock-name[:period[:initial-offset]]"
    )
  )

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
}

/**
  *  Sets a list of symbols that will be rendered during execution
  */
case class SymbolsToWatchAnnotation(symbolNames: Seq[String] = Seq.empty) extends NoTargetAnnotation with TreadleOption

case object SymbolsToWatchAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "tr-symbols-to-watch",
      toAnnotationSeq = (names: Seq[String]) => Seq(SymbolsToWatchAnnotation(names)),
      helpText = "symbol[,symbol[...]"
    )
  )
}

/**
  *  used with treadle's default reset operations
  */
case class ResetNameAnnotation(symbolNames: String = "") extends NoTargetAnnotation with TreadleOption

case object ResetNameAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-reset-name",
      toAnnotationSeq = (resetName: String) => Seq(ResetNameAnnotation(resetName)),
      helpText = "name of the default reset signal"
    )
  )
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object CallResetAtStartupAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-call-reset-at-startup",
      toAnnotationSeq = _ => Seq(CallResetAtStartupAnnotation),
      helpText = "makes treadle do it's own reset at startup, usually for internal use only"
    )
  )
}

/**
  * The circuit used to build a [[TreadleTester]]
  * @param circuit a firrtl ast
  */
case class TreadleCircuitAnnotation(circuit: Circuit) extends NoTargetAnnotation with TreadleOption

/**
  * used to pass parsed firrtl to the TreadleTester
  * @param state the state to be passed along
  */
case class TreadleCircuitStateAnnotation(state: CircuitState) extends NoTargetAnnotation

/**
  * Used to pass a tester on to a test harness
  * @param tester The [[TreadleTester]] to be passed on
  */
case class TreadleTesterAnnotation(tester: TreadleTester) extends NoTargetAnnotation with TreadleOption

/**
  * Factory for [[FirrtlSourceAnnotation]], this is an alias for FirrtlCli
  */
object TreadleFirrtlString extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-firrtl-source-string",
      toAnnotationSeq = (firrtl: String) => Seq(FirrtlSourceAnnotation(firrtl)),
      helpText = "a serialized firrtl circuit, mostly used internally"
    )
  )
}

/**
  * Factory for [[FirrtlFileAnnotation]] annotation, this is an alias for Firrtl Cli
  */
object TreadleFirrtlFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-firrtl-source-file",
      shortOption = Some("tfsf"),
      toAnnotationSeq = (firrtl: String) => Seq(FirrtlFileAnnotation(firrtl)),
      helpText = "specify treadle repl source file"
    )
  )
}

case class BlackBoxFactoriesAnnotation(blackBoxFactories: Seq[ScalaBlackBoxFactory])
        extends NoTargetAnnotation with TreadleOption

/** Constructs this as a registered library that will be auto-detected by
  * projects who have a dependency on Treadle
  */
class TreadleLibrary extends RegisteredLibrary {
  val name: String = "treadle"

  val options: Seq[ShellOption[_]] = Seq(
    WriteVcdAnnotation,
    VcdShowUnderScoredAnnotation,
    VerboseAnnotation,
    AllowCyclesAnnotation,
    RandomSeedAnnotation,
    ShowFirrtlAtLoadAnnotation,
    DontRunLoweringCompilerLoadAnnotation,
    ValidIfIsRandomAnnotation,
    RollBackBuffersAnnotation,
    ClockInfoAnnotation,
    SymbolsToWatchAnnotation,
    ResetNameAnnotation,
    CallResetAtStartupAnnotation,
    TreadleFirrtlString,
    TreadleFirrtlFile
  ).flatMap(_.options)
}

object TreadleDefaults {
  val RollbackBuffers = 0
}
