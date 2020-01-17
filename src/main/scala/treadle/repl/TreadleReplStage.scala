/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.repl

import firrtl.AnnotationSeq
import firrtl.options.{Shell, Stage, StageMain}
import firrtl.stage.FirrtlCli
import logger.Logger
import treadle.TreadleRepl

class TreadleReplStage extends Stage {
  override val shell: Shell = new Shell("treadle-repl") with TreadleReplCli with FirrtlCli

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      val repl = TreadleRepl(annotations)
      repl.run()
    }
    annotations
  }
}

/**
  * This is the primary entry point for running the Treadle Repl
  */
object TreadleReplMain extends StageMain(new TreadleReplStage)
