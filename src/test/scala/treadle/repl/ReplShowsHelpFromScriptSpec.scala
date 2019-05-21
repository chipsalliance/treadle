// See LICENSE for license details.

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

    textOut.contains("show command history") should be (true)
    textOut.contains("load/replace the current firrtl file") should be (true)
  }
}
