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

import firrtl.options.Shell
import treadle._

trait TreadleReplCli { this: Shell =>
  parser.note("TreadleRepl specific options")

  Seq(
    TreadleScriptFile,
    TreadleReplUseVcd,
    TreadleVcdScriptFileOverride,
    TreadleReplDisplayFormat,
    TreadleReplRunScriptAtStartup
  ).foreach(_.addOptions(parser))
}
