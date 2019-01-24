// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasScoptOptions, Unserializable}
import scopt.OptionParser
import treadle.TreadleTester

sealed trait TreadleOption extends Unserializable { this: Annotation => }

case object WriteVcd extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
      .opt[Unit]("tr-write-vcd")
      .abbr("tiwv")
      .abbr("trwv")
      .action( (x, c) => WriteVcd +: c )
      .unbounded()
      .text("write vcd file during simulation")
  }
}

case object VcdShowUnderscored extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
      .opt[Unit]("tr-vcd-show-underscored-vars")
      .abbr("trvsuv")
      .action( (x, c) => VcdShowUnderscored +: c )
      .unbounded()
      .text("vcd output by default does not show var that start with _T_ or _Gen_, this overrides that")
  }
}

case object SetVerbose extends NoTargetAnnotation with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-verbose")
            .abbr("tv")
            .abbr("trv")
            .action( (x, c) => SetVerbose +: c )
            .unbounded()
            .text("makes engine very verbose")
  }
}

case object AllowCycles extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-allow-cycles")
            .abbr("trac")
            .action( (x, c) => AllowCycles +: c )
            .unbounded()
            .text("allow combinational loops to be processed")
  }
}

case class RandomSeed(seed: Double) extends NoTargetAnnotation with TreadleOption

case object RandomSeed extends HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Double]("tr-random-seed")
            .abbr("trrs")
            .action( (x, c) => RandomSeed(x) +: c )
            .unbounded()
            .text("seed used for random numbers generated for tests, default is current time in ms")
  }
}

case object ShowFirrtlAtLoad extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-show-firrtl-at-load")
            .abbr("trsfal")
            .action( (x, c) => ShowFirrtlAtLoad +: c )
            .unbounded()
            .text("show the compiled low firrtl at tester load time")
  }
}

case object LowCompileAtLoad extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-dont-run-lower-compiler-on-load")
            .abbr("trlcol")
            .action( (x, c) => LowCompileAtLoad +: c )
            .unbounded()
            .text("disable running of lowering compiler when firrtl is ingested")
  }
}

case object ValidIfIsRandom extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-valid-if-is-random")
            .abbr("trviir")
            .action( (x, c) => ValidIfIsRandom +: c )
            .unbounded()
            .text("default validIf returns valid regardless of valid, this randomizes instead")
  }
}

case class RollbackBuffers(number: Int) extends NoTargetAnnotation

object RollbackBuffers extends HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Int]("tr-rollback-buffers")
            .abbr("trrb")
            .action( (x, c) => RollbackBuffers(x) +: c )
            .unbounded()
            .text("number of rollback buffers, 0 (i.e. no rollback buffers) is the default")
  }
}

case class ClockInfoList(clockInfoSeq: Seq[String]) extends NoTargetAnnotation

object ClockInfoList extends HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Seq[String]]("tr-clock-info")
            .abbr("trci")
            .action( (x, c) => ClockInfoList(x) +: c )
            .unbounded()
            .text("One or more of clock-name[:period[:initial-offset]] (use comma separator")
  }
}

case class ResetName(name: String) extends NoTargetAnnotation

object ResetName extends HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[String]("tr-reset-name")
            .abbr("trrn")
            .action( (x, c) => ResetName(x) +: c )
            .unbounded()
            .text("name of default/top-level reset")
  }
}

case object CallResetAtStartup extends NoTargetAnnotation with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-call-reset-at-start")
            .abbr("trcras")
            .action( (x, c) => CallResetAtStartup +: c )
            .unbounded()
            .text("makes engine very verbose")
  }
}

case class SymbolsToWatch(symbolNames: Seq[String]) extends NoTargetAnnotation

object SymbolsToWatch extends HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Seq[String]]("tr-symbols-to-watch")
            .abbr("trstw")
            .action( (x, c) => SymbolsToWatch(x) +: c )
            .unbounded()
            .text("name of default/top-level reset")
  }
}

case class BlackBoxFactories(factories: BlackBoxFactories) extends NoTargetAnnotation

case class TreadleTesterAnnotation(tester: TreadleTester) extends NoTargetAnnotation with Unserializable
