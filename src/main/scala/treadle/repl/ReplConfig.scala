// See LICENSE for license details.

package treadle.repl

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasScoptOptions, OptionsView, RegisteredLibrary}
import scopt.OptionParser

case class ReplExecutionOptions(
  firrtlSourceName : String = "",
  scriptName       : String = "",
  firrtlSource     : String = "",
  useVcdScript     : Boolean = false,
  vcdScriptOverride: String = "",
  runScriptAtStart : Boolean = false,
  outputFormat     : String = "d"
)

object ReplViewer {
  implicit object ReplOptionsView extends OptionsView[ReplExecutionOptions] {
    def view(options: AnnotationSeq): Option[ReplExecutionOptions] = {
      val executionOptions = options.foldLeft(ReplExecutionOptions()) { (previousOptions, annotation) =>
        annotation match {
          case ReplSourceNameAnnotation(name)                  => previousOptions.copy(firrtlSourceName = name)
          case ReplFirrtlAnnotation(name)                      => previousOptions.copy(firrtlSource = name)
          case ReplScriptNameAnnotation(name)                  => previousOptions.copy(scriptName = name)
          case ReplUseVcdScriptAnnotation                      => previousOptions.copy(useVcdScript = true)
          case ReplScriptNameAnnotation(name)                  => previousOptions.copy(scriptName = name)
          case _ => previousOptions
        }

      }
      Some(executionOptions)
    }
  }
}

sealed trait ReplOption extends HasScoptOptions

case class ReplSourceNameAnnotation(name: String = "") extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("repl-source-file")
    .abbr("r-sf")
    .action( (name, c) => c :+ ReplSourceNameAnnotation(name) )
    .unbounded()
    .text("firrtl source for repl to operate on")
}

case class ReplFirrtlAnnotation(name: String = "") extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("repl-source")
    .abbr("r-s")
    .action( (firrtl, c) => c :+ ReplFirrtlAnnotation(firrtl) )
    .unbounded()
    .text("firrtl source for repl to operate on")
}

case class ReplScriptNameAnnotation(name: String = "") extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("repl-script-name")
    .abbr("r-sf")
    .action( (name, c) => c :+ ReplScriptNameAnnotation(name) )
    .unbounded()
    .text("script file to load")
}

case object ReplUseVcdScriptAnnotation extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("repl-use-vcd")
    .abbr("r-sf")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("script file to load")
}

case class ReplVcdScriptOverrideAnnotation(name: String = "") extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("repl-vcd-script-override")
    .abbr("r-vso")
    .action( (firrtl, c) => c :+ ReplVcdScriptOverrideAnnotation(firrtl) )
    .unbounded()
    .text("override default name for vcd input file")
}

case object ReplRunScriptOnStartupAnnotation extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("repl-run-script-on-startup")
    .abbr("r-rsos")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("run the script automatically at start-up")
}

case class ReplOutputFormatAnnotation(name: String = "d") extends NoTargetAnnotation with ReplOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("repl-output-format")
    .abbr("r-of")
    .action( (format, c) => c :+ ReplOutputFormatAnnotation(format) )
    .unbounded()
    .text("default output format when display wire values b, d, x")
}

object TreadleReplLibrary extends RegisteredLibrary {
  override def name: String = "treadle-repl"

  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      ReplSourceNameAnnotation(),
      ReplFirrtlAnnotation(),
      ReplScriptNameAnnotation(),
      ReplUseVcdScriptAnnotation,
      ReplVcdScriptOverrideAnnotation(),
      ReplRunScriptOnStartupAnnotation,
      ReplOutputFormatAnnotation()
    )

    seq.foreach(_.addOptions(parser))
  }
}


