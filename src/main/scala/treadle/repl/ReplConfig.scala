// See LICENSE for license details.

package treadle.repl

import firrtl.ExecutionOptionsManager

case class ReplConfig(
  firrtlSourceName : String = "",
  scriptName       : String = "",
  firrtlSource     : String = "",
  useVcdScript     : Boolean = false,
  vcdScriptOverride: String = "",
  runScriptAtStart : Boolean = false,
  outputFormat     : String = "d"
)
        extends firrtl.ComposableOptions

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
