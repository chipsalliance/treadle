// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasScoptOptions, RegisteredLibrary}
import scopt.OptionParser
import treadle.executable.{ClockInfo, TreadleException}

sealed trait TreadleOption extends HasScoptOptions

/**
  * Tells treadle to write a vcd file during simulation
  */
case object WriteVcdAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-write-vcd")
    .abbr("tiwv")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("writes vcd executioin log, filename will be based on top-name")
}

/**
  * Tells treadle to include _T_* and _GEN_* wires in VCD output
  */
case object VcdShowUnderScoredAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-vcd-show-underscored-vars")
    .abbr("tivsu")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("vcd output by default does not show var that start with underscore, this overrides that")
}

/**
  *  Tells treadle to execute verbosely
  */
case object VerboseAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-verbose")
    .abbr("tv")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("makes the treadle very verbose")
}

/**
  *  Tells treadle to allow cycles
  */
case object AllowCyclesAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-allow-cycle")
    .abbr("tac")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("will try to run when firrtl contains combinational loops")
}

/**
  *  Sets the seed for treadle's private random number generator
  */
case class RandomSeedAnnotation(seed: Long = 0L) extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Long]("tr-random-seed")
    .abbr("trc")
    .action( (x, c) => c :+ RandomSeedAnnotation(x) )
    .unbounded()
    .text("will try to run when firrtl contains combinational loops")
}

/**
  *  Tells treadle to show the low firrtl it is starting out with
  */
case object ShowFirrtlAtLoadAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-show-firrtl-at-load")
    .abbr("tsfal")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("show the low firrtl source treadle is using to build simulator")
}

/**
  *  Tells treadle to not run its own lowering pass on firrtl input (not recommended)
  */
case object DontRunLoweringCompilerLoadAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-dont-run-lower-compiler-on-load")
    .abbr("tdrlcol")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("do not run its own lowering pass on firrtl input (not recommended)")
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object ValidIfIsRandomAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-validif-random")
    .abbr("tvir")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("validIf returns random value when condition is false")
}

/**
  *  Sets the number of rollback buffers in simulator, useful to see why wires have their valies
  */
//scalastyle:off magic.number
case class RollBackBuffersAnnotation(rollbackBufferDepth: Int = 4) extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Int]("tr-rollback-buffers")
    .abbr("trb")
    .action( (x, c) => c :+ RollBackBuffersAnnotation(x) )
    .unbounded()
    .text("number of rollback buffers, 0 is no buffers, default is 4")
}

/**
  *  Sets one or more clocks including their frequencies and phase
  */
case class ClockInfoAnnotation(clockInfoSeq: ClockInfo = ClockInfo()) extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("tr-clock-info")
    .abbr("tci")
    .action( (x, c) => c :+ ClockInfoAnnotation(parseClockInfo(x)) )
    .unbounded()
    .text("clock-name[:period[:initial-offset]]")

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
case class SymbolsToWatchAnnotation(
  symbolNames: Seq[String] = Seq.empty
) extends NoTargetAnnotation with TreadleOption {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Seq[String]]("tr-symbols-to-watch")
    .abbr("tstw")
    .action( (x, c) => c :+ SymbolsToWatchAnnotation(x) )
    .unbounded()
    .text("symbol[,symbol[...]")
}

/**
  *  Sets the seed for treadle's private random number generator
  */
case class ResetNameAnnotation(symbolNames: String = "") extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("tr-reset-name")
    .abbr("trn")
    .action( (x, c) => c :+ ResetNameAnnotation(x) )
    .unbounded()
    .text("name of the default reset signal")
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object CallResetAtStartupAnnotation extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("tr-call-reset-at-startup")
    .abbr("tcras")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("makes treadle do it's own reset at startup, usually for internal use only")
}

case class TreadleFirrtlString(firrtl: String = "") extends NoTargetAnnotation with TreadleOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("tr-firrtl-source-string")
    .abbr("trns")
    .action( (x, c) => c :+ TreadleFirrtlString(x) )
    .unbounded()
    .text("a serialized firrtl circuit, mostly used internally")
}

case class BlackBoxFactoriesAnnotation(blackBoxFactories: Seq[ScalaBlackBoxFactory]) extends NoTargetAnnotation

object TreadleLibrary extends RegisteredLibrary {
  override def name: String = "treadle"

  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      WriteVcdAnnotation,
      VcdShowUnderScoredAnnotation,
      VerboseAnnotation,
      AllowCyclesAnnotation,
      RandomSeedAnnotation(),
      ShowFirrtlAtLoadAnnotation,
      DontRunLoweringCompilerLoadAnnotation,
      ValidIfIsRandomAnnotation,
      RollBackBuffersAnnotation(),
      ClockInfoAnnotation(),
      SymbolsToWatchAnnotation(),
      ResetNameAnnotation(),
      CallResetAtStartupAnnotation,
      TreadleFirrtlString()
    )

    seq.foreach(_.addOptions(parser))
  }
}
