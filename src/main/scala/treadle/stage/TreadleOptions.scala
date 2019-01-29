// See LICENSE for license details.

package treadle.stage

import treadle.ScalaBlackBoxFactory
import treadle.executable.ClockInfo

case class TreadleOptions (
  writeVCD             : Boolean                   = false,
  vcdShowUnderscored   : Boolean                   = false,
  setVerbose           : Boolean                   = false,
  allowCycles          : Boolean                   = false,
  randomSeed           : Long                      = System.currentTimeMillis(),
  blackBoxFactories    : Seq[ScalaBlackBoxFactory] = Seq.empty,
  showFirrtlAtLoad     : Boolean                   = false,
  lowCompileAtLoad     : Boolean                   = true,
  validIfIsRandom      : Boolean                   = false,
  rollbackBuffers      : Int                       = 10,
  clockInfo            : Seq[ClockInfo]            = Seq.empty,
  resetName            : String                    = "reset",
  callResetAtStartUp   : Boolean                   = false,
  symbolsToWatch       : Seq[String]               = Seq.empty
)