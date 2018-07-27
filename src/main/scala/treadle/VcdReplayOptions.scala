// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasScoptOptions, OptionsView, RegisteredLibrary}
import scopt.OptionParser

case class VcdReplayExecutionOptions(
  firrtlSourceName:     String = "",
  vcdSourceName:        String = "",
  skipEvents:           Int = 0,
  eventsToRun:          Int = -1,
  testAliasedWires:     Boolean = false
)

object VcdReplayOptionsViewer {
  implicit object VcdReplayOptionsView extends OptionsView[VcdReplayExecutionOptions] {
    def view(options: AnnotationSeq): Option[VcdReplayExecutionOptions] = {
      val executionOptions = options.foldLeft(VcdReplayExecutionOptions()) { (previousOptions, annotation) =>
        annotation match {
          case VcdReplayFirrtlSourceNameAnnotation(name) => previousOptions.copy(firrtlSourceName = name)
          case VcdReplayVcdFileAnnotation(name)          => previousOptions.copy(vcdSourceName = name)
          case VcdReplaySkipEventsAnnotation(events)     => previousOptions.copy(skipEvents = events)
          case VcdReplayEventsToRunAnnotation(events)    => previousOptions.copy(eventsToRun = events)
          case VcdReplayTestAliasedWiresAnnotation       => previousOptions.copy(testAliasedWires = true)
          case _ => previousOptions
        }

      }
      Some(executionOptions)
    }
  }
}



sealed trait VcdReplayOption extends HasScoptOptions

case class VcdReplayFirrtlSourceNameAnnotation(name: String = "") extends NoTargetAnnotation with VcdReplayOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-replay-firrtl-file")
    .abbr("vr-ff")
    .action( (name, c) => c :+ VcdReplayFirrtlSourceNameAnnotation(name) )
    .unbounded()
    .text("firrtl source for vcd replay to operate on")
}

case class VcdReplayVcdFileAnnotation(name: String = "") extends NoTargetAnnotation with VcdReplayOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-replay-vcd-file")
    .abbr("vr-vf")
    .action( (name, c) => c :+ VcdReplayVcdFileAnnotation(name) )
    .unbounded()

    .text("vcd source for vcd replay to operate on")
}

case class VcdReplaySkipEventsAnnotation(skip: Int = 0) extends NoTargetAnnotation with VcdReplayOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Int]("vcd-replay-skip-events")
    .abbr("vr-vf")
    .action( (skip, c) => c :+ VcdReplaySkipEventsAnnotation(skip) )
    .unbounded()

    .text("skip this many events")
}

case class VcdReplayEventsToRunAnnotation(events: Int = 0) extends NoTargetAnnotation with VcdReplayOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Int]("vcd-replay-events-to-run")
    .abbr("vr-evtr")
    .action( (events, c) => c :+ VcdReplayEventsToRunAnnotation(events) )
    .unbounded()

    .text("events to run")
}

case object VcdReplayTestAliasedWiresAnnotation extends NoTargetAnnotation with VcdReplayOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("vcd-replay-test-aliased-wires")
    .abbr("vr-evtr")
    .action( (_, c) => c :+ VcdReplayTestAliasedWiresAnnotation )
    .unbounded()

    .text("test aliased wires during execution")
}

object VcdReplayLibrary extends RegisteredLibrary {
  override def name: String = "vcd-replay"

  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      VcdReplayFirrtlSourceNameAnnotation(),
      VcdReplayVcdFileAnnotation(),
      VcdReplaySkipEventsAnnotation(),
      VcdReplayEventsToRunAnnotation(),
      VcdReplayTestAliasedWiresAnnotation
    )

    seq.foreach(_.addOptions(parser))
  }
}
