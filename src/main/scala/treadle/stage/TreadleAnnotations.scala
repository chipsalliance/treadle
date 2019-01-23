// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasScoptOptions, Unserializable}
import scopt.OptionParser

sealed trait TreadleOption extends Unserializable { this: Annotation => }

case object WriteVcd extends NoTargetAnnotation with TreadleOption with HasScoptOptions {

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    p
      .opt[Unit]("tr-write-vcd")
      .abbr("trwv")
      .action( (x, c) => WriteVcd +: c )
      .unbounded()
      .text("write vcd file during simulation")
  }
}