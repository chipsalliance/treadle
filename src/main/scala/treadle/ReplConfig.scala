// See LICENSE for license details.

package treadle

import firrtl.{ExecutionOptionsManager, HasFirrtlExecutionOptions}
import firrtl.annotations.{Annotation, SingleStringAnnotation, NoTargetAnnotation}

sealed trait ReplOption { self: Annotation => }
case class ScriptNameAnnotation(value: String) extends SingleStringAnnotation with ReplOption
case object UseVcdScriptAnnotation extends NoTargetAnnotation with ReplOption
case class VcdScriptOverrideAnnotation(value: String) extends SingleStringAnnotation with ReplOption
case object RunScriptAtStartAnnotation extends NoTargetAnnotation with ReplOption
case class OutputFormatAnnotation(value: String) extends SingleStringAnnotation with ReplOption

case class ReplConfig(
  scriptName       : Option[String] = None,
  useVcdScript     : Boolean = false,
  vcdScriptOverride: Option[String] = None,
  runScriptAtStart : Boolean = false,
  outputFormat     : String = "d"
)

trait HasReplConfig {
  self: ExecutionOptionsManager with HasFirrtlExecutionOptions =>

  lazy val replConfig: ReplConfig = options
    .collect{ case r: ReplOption => r }
    .foldLeft(ReplConfig())( (r, x) =>
      x match {
        case ScriptNameAnnotation(n)        => r.copy(scriptName = Some(n))
        case UseVcdScriptAnnotation         => r.copy(useVcdScript = true)
        case VcdScriptOverrideAnnotation(s) => r.copy(vcdScriptOverride = Some(s))
        case RunScriptAtStartAnnotation     => r.copy(runScriptAtStart = true)
        case OutputFormatAnnotation(f)      => r.copy(outputFormat = f)
      }
    )

  parser.note("firrtl-repl")

  parser.opt[String]("fr-script-file")
    .abbr("frsf")
    .valueName("<firrtl-script-file>")
    .action( (x, a) => a :+ ScriptNameAnnotation(x) )
    .text("script file to load on startup, default is no file")

  parser.opt[Unit]("fr-use-vcd-script")
    .abbr("fruvs")
    .action( (_, a) => a :+ UseVcdScriptAnnotation )
    .text("load vcd file as script, default is false")

  parser.opt[String]("fr-vcd-script-override")
    .abbr("frvso")
    .valueName("<vcd-file>")
    .action( (x, a) => a :+ VcdScriptOverrideAnnotation(x) )
    .text("load vcd file as script, default is false")

  parser.opt[Unit]("fr-run-script-on-startup")
    .abbr("frrsos")
    .valueName("<vcd-file>")
    .action( (_, a) => a :+ RunScriptAtStartAnnotation )
    .text("run script immediately on startup")

  parser.opt[String]("fr-display-format")
    .abbr("frdf")
    .valueName("format")
    .action( (x, a) => a :+ OutputFormatAnnotation(x) )
    .text("how to display values d - decimal, x - hex, b - binary")

  def getVcdFileName: String = {
    self.getBuildFileName("vcd", replConfig.vcdScriptOverride)
  }
}
