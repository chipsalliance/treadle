// See LICENSE for license details.

package treadle.stage

import firrtl.options.Shell
import treadle.{AllowCyclesAnnotation, CallResetAtStartupAnnotation, ClockInfoAnnotation, DontRunLoweringCompilerLoadAnnotation, RandomSeedAnnotation, ResetNameAnnotation, RollBackBuffersAnnotation, ShowFirrtlAtLoadAnnotation, SymbolsToWatchAnnotation, TreadleFirrtlFile, TreadleFirrtlString, ValidIfIsRandomAnnotation, VcdShowUnderScoredAnnotation, VerboseAnnotation, WriteVcdAnnotation}

trait TreadleCli { this: Shell =>
  parser.note("Treadle specific options")

  Seq(
    WriteVcdAnnotation,
    VcdShowUnderScoredAnnotation,
    VerboseAnnotation,
    AllowCyclesAnnotation,
    RandomSeedAnnotation,
    ShowFirrtlAtLoadAnnotation,
    DontRunLoweringCompilerLoadAnnotation,
    ValidIfIsRandomAnnotation,
    RollBackBuffersAnnotation,
    ClockInfoAnnotation,
    SymbolsToWatchAnnotation,
    ResetNameAnnotation,
    CallResetAtStartupAnnotation,
    TreadleFirrtlString,
    TreadleFirrtlFile
  ).foreach(_.addOptions(parser))
}
