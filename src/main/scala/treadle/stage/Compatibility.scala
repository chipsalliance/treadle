// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import treadle.{HasTreadleSuite, TreadleOptionsManager}

object Compatibility {
  def toAnnotations(optionsManager: HasTreadleSuite): AnnotationSeq = {
    //TODO: build this out for backward compatibility
    Seq.empty
  }
}
