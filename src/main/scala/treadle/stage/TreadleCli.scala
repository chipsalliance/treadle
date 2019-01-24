// See LICENSE for license details.

package treadle.stage

import firrtl.options.{HasScoptOptions, Shell}

trait TreadleCli { this: Shell =>
  parser.note("Treadle Front End Options")

  val commandLineAnnotations: Seq[HasScoptOptions] = Seq(
    WriteVcd,
    VcdShowUnderscored,
    SetVerbose,
    AllowCycles,
    RandomSeed,
    ShowFirrtlAtLoad,
    LowCompileAtLoad,
    ValidIfIsRandom,
    RollbackBuffers,
    ClockInfoList,
    ResetName,
    CallResetAtStartup,
    SymbolsToWatch,
  )
  commandLineAnnotations.foreach(_.addOptions(parser))
}
