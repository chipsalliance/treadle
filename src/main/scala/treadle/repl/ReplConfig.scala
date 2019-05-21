// See LICENSE for license details.

package treadle.repl

import java.io.File

import firrtl.stage.FirrtlFileAnnotation
import firrtl.{AnnotationSeq, ExecutionOptionsManager}

case class ReplConfig(
  basePath         : String = "",
  baseName         : String = "",
  suffix           : String = "",
  firrtlSourceName : String = "",
  scriptName       : String = "",
  firrtlSource     : String = "",
  useVcdScript     : Boolean = false,
  vcdScriptOverride: String = "",
  runScriptAtStart : Boolean = false,
  outputFormat     : String = "d"
) {
  def getVcdInputFileName: String = {
    if(vcdScriptOverride.nonEmpty) {
      vcdScriptOverride
    }
    else {
      basePath + File.pathSeparator + baseName + suffix
    }
  }
}


//scalastyle:off cyclomatic.complexity
case object ReplConfig {
  def fromAnnotations(annotationSeq: AnnotationSeq): ReplConfig = {
    annotationSeq.foldLeft(ReplConfig()) { case (config, annotation) =>
      annotation match {
        case TreadleScriptFile(s)            => config.copy(scriptName = s)
        case TreadleReplRunScriptAtStartup   => config.copy(runScriptAtStart = true)
        case TreadleReplUseVcd               => config.copy(useVcdScript = true)
        case TreadleVcdScriptFileOverride(s) => config.copy(vcdScriptOverride = s)
        case TreadleReplDisplayFormat(s)     => config.copy(outputFormat = s)
        case FirrtlFileAnnotation(s)               =>
          val f = new File(s)
          val basePath = f.getCanonicalFile.getParentFile.getAbsolutePath
          val name = f.getName
          val (baseName, suffix) = name.split(".+?/(?=[^/]+$)").toList match {
            case a :: Nil => (a, "")
            case a :: b :: Nil => ("", "")
            case _ => ("", "")
          }
          config.copy(basePath = basePath, baseName = baseName, suffix = suffix, firrtlSourceName = s)
        case other =>
          config
      }
    }
  }
}

trait HasReplConfig {
  self: ExecutionOptionsManager =>

  var replConfig = ReplConfig()

  parser.note("firrtl-repl")

  parser.opt[String]("fr-firrtl-source")
    .abbr("frfs")
    .valueName("<firrtl-source-file>")
    .foreach { x =>
      replConfig = replConfig.copy(firrtlSourceName = x)
    }
    .text("firrtl file to load on startup, default is no file")

  parser.opt[String]("fr-script-file")
    .abbr("frsf")
    .valueName("<firrtl-script-file>")
    .foreach { x =>
      replConfig = replConfig.copy(scriptName = x)
    }
    .text("script file to load on startup, default is no file")

  parser.opt[Unit]("fr-use-vcd-script")
    .abbr("fruvs")
    .foreach { _ =>
      replConfig = replConfig.copy(useVcdScript = true)
    }
    .text("load vcd file as script, default is false")

  parser.opt[String]("fr-vcd-script-override")
    .abbr("frvso")
    .valueName("<vcd-file>")
    .foreach { x =>
      replConfig = replConfig.copy(vcdScriptOverride = x, useVcdScript = true)
    }
    .text("load vcd file as script, default is false")

  parser.opt[String]("fr-display-format")
    .abbr("frdf")
    .valueName("format")
    .foreach { x =>
      replConfig = replConfig.copy(outputFormat = x)
    }
    .text("how to display values d - decimal, x - hex, b - binary")

  parser.opt[Unit]("fr-run-script-on-startup")
    .abbr("frrsos")
    .valueName("<vcd-file>")
    .foreach { _ =>
      replConfig = replConfig.copy(runScriptAtStart = true)
    }
    .text("run script immediately on startup")

  def getVcdFileName: String = {
    self.getBuildFileName("vcd", replConfig.vcdScriptOverride)
  }
}
