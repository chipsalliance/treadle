// See LICENSE for license details.

package treadle.vcd

import firrtl.options.StageMain
import treadle.vcd.diff._

object VCDDiff extends StageMain(new VcdDiffStage)
