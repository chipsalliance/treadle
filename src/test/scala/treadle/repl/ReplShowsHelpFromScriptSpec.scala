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

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.FileUtils
import firrtl.options.TargetDirAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.TreadleRepl

class ReplShowsHelpFromScriptSpec extends FreeSpec with Matchers {
  "start a repl, run a script containing commands help ; quit" in {
    val targetDir = "test_run_dir/repl/basic-repl-test"
    val replInputFile = targetDir + File.separator + "repl.in"

    FileUtils.makeDirectory(targetDir)
    val printFile = new PrintStream(new File(replInputFile))
    printFile.println("help")
    printFile.println("quit")
    printFile.close()

    val output = new ByteArrayOutputStream()

    val annotations = Seq(
      OverrideOutputStream(output),
      TargetDirAnnotation(targetDir),
      TreadleScriptFile(replInputFile),
      TreadleReplRunScriptAtStartup
    )
    Console.withOut(new PrintStream(output)) {
      val repl = TreadleRepl(annotations)
      repl.run()
    }

    val textOut = output.toString()

    // println(textOut)

    textOut.contains("show command history") should be(true)
    textOut.contains("load/replace the current firrtl file") should be(true)
  }
}
