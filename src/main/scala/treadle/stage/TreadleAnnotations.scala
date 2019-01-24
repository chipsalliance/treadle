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



case object SetVerbose extends NoTargetAnnotation with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
            .opt[Unit]("tr-verbose")
            .abbr("tv")
            .action( (x, c) => SetVerbose +: c )
            .unbounded()
            .text("makes engine very verbose")
  }
}
case class TreadleTesterAnnotation(tester: TreadleTester) extends NoTargetAnnotation with Unserializable
