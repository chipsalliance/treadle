// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.{Shell, Stage}
import firrtl.stage.{FirrtlCli, FirrtlStage}
import logger.{Logger, LoggerCli}

object TreadleStage extends Stage {
  val shell: Shell = new Shell("chisel") with TreadleCli with LoggerCli with FirrtlCli

  def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      annotations
    }
  }
}
