// See LICENSE for license details.

package treadle.repl

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.FileUtils
import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.TreadleRepl

class GcdInReplSpec extends FreeSpec with Matchers {
  "run gcd to compute gcd(8,12) => 4" in {
    val targetDir = "test_run_dir/repl/gcd-test"
    val replInputFile = targetDir + File.separator + "gcd.in"

    val stream = getClass.getResourceAsStream("/GCD.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    FileUtils.makeDirectory(targetDir)
    val printFile = new PrintStream(new File(replInputFile))
    printFile.println("poke io_a 8 ; poke io_b 12; poke io_e 1 ; step")
    printFile.println("poke io_e 0")
    printFile.println("waitfor io_v 1")
    printFile.println("peek io_z")
        printFile.println("quit")
    printFile.close()

    val output = new ByteArrayOutputStream()

    val annotations = Seq(
      FirrtlSourceAnnotation(input),
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

    println(textOut)

    textOut.contains("io_v == value 1 in 3 cycle") should be (true)
    textOut.contains("peek io_z 4") should be (true)

  }
}
