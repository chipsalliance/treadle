// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.options.OptionsView
import treadle.executable.ClockInfo

//scalastyle:off magic.number

case class TreadleExecutionOptions(
  writeVCD: Boolean                            = false,
  vcdShowUnderscored: Boolean                  = false,
  setVerbose: Boolean                          = false,
  allowCycles: Boolean                         = false,
  randomSeed: Long                             = System.currentTimeMillis(),
  blackBoxFactories: Seq[ScalaBlackBoxFactory] = Seq.empty,
  showFirrtlAtLoad: Boolean                    = false,
  lowCompileAtLoad: Boolean                    = true,
  validIfIsRandom: Boolean                     = false,
  rollbackBuffers: Int                         = 4,
  clockInfo: Seq[ClockInfo]                    = Seq.empty,
  resetName: String                            = "reset",
  callResetAtStartUp: Boolean                  = false,
  symbolsToWatch: Seq[String]                  = Seq.empty
)

//scalastyle:off cyclomatic.complexity
object TreadleViewer {
  implicit object TreadleOptionsView extends OptionsView[TreadleExecutionOptions] {
    def view(options: AnnotationSeq): Option[TreadleExecutionOptions] = {
      val executionOptions = options.foldLeft(TreadleExecutionOptions()) { (previousOptions, annotation) =>
        annotation match {
          case WriteVcdAnnotation                    => previousOptions.copy(writeVCD = true)
          case VcdShowUnderScoredAnnotation          => previousOptions.copy(vcdShowUnderscored = true)
          case VerboseAnnotation                     => previousOptions.copy(setVerbose = true)
          case AllowCyclesAnnotation                 => previousOptions.copy(allowCycles = true)
          case RandomSeedAnnotation(seed)            => previousOptions.copy(randomSeed = seed)
          case ShowFirrtlAtLoadAnnotation            => previousOptions.copy(showFirrtlAtLoad = true)
          case DontRunLoweringCompilerLoadAnnotation => previousOptions.copy(lowCompileAtLoad = true)
          case ValidIfIsRandomAnnotation             => previousOptions.copy(validIfIsRandom = true)
          case RollBackBuffersAnnotation(buffers)    => previousOptions.copy(rollbackBuffers = buffers)
          case ResetNameAnnotation(name)             => previousOptions.copy(resetName = name)
          case CallResetAtStartupAnnotation          => previousOptions.copy(callResetAtStartUp = true)
          case BlackBoxFactoriesAnnotation(seq)      =>
            previousOptions.copy(blackBoxFactories = previousOptions.blackBoxFactories ++ seq)
          case ClockInfoAnnotation(clockInfo) =>
            previousOptions.copy(clockInfo = previousOptions.clockInfo :+ clockInfo)
          case SymbolsToWatchAnnotation(symbols) =>
            previousOptions.copy(symbolsToWatch = previousOptions.symbolsToWatch ++ symbols)
          case _ => previousOptions
        }

      }
      Some(executionOptions)
    }
  }
}

