// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.options.OptionsView
import treadle.executable.ClockInfo

package object stage {

  implicit object TreadleConfigView extends OptionsView[TreadleOptions] {

    //scalastyle:off cyclomatic.complexity
    def view(options: AnnotationSeq): TreadleOptions = {
      options
              .foldLeft(TreadleOptions()) { (c, x) =>
                x match {
                  case WriteVcd => c.copy(writeVCD = true)
                  case VcdShowUnderscored => c.copy(vcdShowUnderscored = true)
                  case SetVerbose => c.copy(setVerbose = true)
                  case AllowCycles => c.copy(allowCycles = true)
                  case RandomSeed(seed) => c.copy(randomSeed = seed.toLong)
                  case BlackBoxFactories(factories) => c.copy(blackBoxFactories = factories)
                  case ShowFirrtlAtLoad => c.copy(showFirrtlAtLoad = true)
                  case ValidIfIsRandom => c.copy(validIfIsRandom = true)
                  case RollbackBuffers(buffers) => c.copy(rollbackBuffers = buffers)
                  case CallResetAtStartup => c.copy(callResetAtStartUp = true)
                  case ResetName(name) => c.copy(resetName = name)
                  case SymbolsToWatch(list) => c.copy(symbolsToWatch = list)
                  case ClockInfoList(list) =>
                    c.copy(clockInfo = list.map(ClockInfo.parseClockInfo(_)))
                  case _ => c
                }
              }
    }
  }

}
