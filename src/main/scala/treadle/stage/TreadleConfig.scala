// See LICENSE for license details.

package treadle.stage

import treadle.ScalaBlackBoxFactory
import treadle.executable.ClockInfo

case class TreadleConfig (
  writeVcd             : Boolean                   = false,
  vcdShowUnderscored   : Boolean                   = false,
  setVerbose           : Boolean                   = false,
  setOrderedExec       : Boolean                   = false,
  allowCycles          : Boolean                   = false,
  randomSeed           : Long                      = System.currentTimeMillis(),
  blackBoxFactories    : Seq[ScalaBlackBoxFactory] = Seq.empty,
  maxExecutionDepth    : Long                      = Int.MaxValue,
  showFirrtlAtLoad     : Boolean                   = false,
  lowCompileAtLoad     : Boolean                   = true,
  validIfIsRandom      : Boolean                   = false,
  rollbackBuffers      : Int                       = 10,
  clockInfo            : Seq[ClockInfo]            = Seq.empty,
  resetName            : String                    = "reset",
  callResetAtStartUp   : Boolean                   = false,
  symbolsToWatch       : Seq[String]               = Seq.empty
)