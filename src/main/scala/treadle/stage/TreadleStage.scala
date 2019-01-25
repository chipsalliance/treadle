// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, Shell, Stage}
import firrtl.stage.FirrtlCli
import logger.{Logger, LoggerCli}
import treadle.stage.phases.ConstructTester

object TreadleStage extends Stage {
  val shell: Shell = new Shell("chisel") with TreadleCli with LoggerCli with FirrtlCli

  private val phases: Seq[Phase] = Seq(
    ConstructTester,
  )

  def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      phases.foldLeft(annotations) { (annos, phase) => phase.transform(annos) }
    }
  }
}
