// See LICENSE for license details.

package treadle.repl

import firrtl.options.Shell
import treadle._

trait TreadleReplCli { this: Shell =>
  parser.note("TreadleRepl specific options")

  Seq(
    FirrtlFileName,
    TreadleScriptFile,
    TreadleReplUseVcd,
    TreadleVcdScriptFileOverride,
    TreadleReplDisplayFormat,
    TreadleReplRunScriptAtStartup
  ).foreach(_.addOptions(parser))
}
