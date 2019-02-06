// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import treadle.HasTreadleSuite

object Compatibility {
  def toAnnotations(optionsManager: HasTreadleSuite): AnnotationSeq = {

    def makeOpt(condition: Boolean)(annotation: NoTargetAnnotation): Option[NoTargetAnnotation] = {
      if(condition) {
        Some(annotation)
      }
      else {
        None
      }
    }
    val to = optionsManager.treadleOptions

    val annotations = Seq(
      to.writeVCD                         -> WriteVcd,
      to.vcdShowUnderscored               -> VcdShowUnderscored,
      to.setVerbose                       -> SetVerbose,
      to.allowCycles                      -> AllowCycles,
      true                                -> RandomSeed(to.randomSeed),
      to.blackBoxFactories.nonEmpty       -> BlackBoxFactories(to.blackBoxFactories),
      to.showFirrtlAtLoad                 -> ShowFirrtlAtLoad,
      to.lowCompileAtLoad                 -> LowCompileAtLoad,
      to.validIfIsRandom                  -> ValidIfIsRandom,
      to.callResetAtStartUp               -> CallResetAtStartup,
      (to.rollbackBuffers > 0)            -> RollbackBuffers(to.rollbackBuffers),
      to.clockInfo.nonEmpty               -> ClockInfoList(to.clockInfo.map(_.toString)),
      (to.resetName != "reset")           -> ResetName(to.resetName),
      to.symbolsToWatch.nonEmpty          -> SymbolsToWatch(to.symbolsToWatch)

    ).flatMap { case (condition, annotation) => makeOpt(condition)(annotation)}

    annotations
  }
}
