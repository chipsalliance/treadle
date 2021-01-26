// SPDX-License-Identifier: Apache-2.0

package treadle

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.FileUtils
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.utils.CoveragePrettyPrinterMain

class FormalCoverSpec extends AnyFreeSpec with Matchers {
  private val stream = getClass.getResourceAsStream("/HasCoverStatements.fir")
  private val firrtlSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

  "cover statements should be counted" in {
    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      val c0 = tester.getCoverage().toMap
      assert(c0.size == 6, "There are 6 cover statements in HasCoverStatements.fir")
      assert(c0.keys.count(_.startsWith("c.")) == 2, "There are two cover statements in the submodule.")
      c0.values.foreach(v => assert(v == 0, "All count should be zero since we have not taken a step yet"))

      tester.step(10)

      val c1 = tester.getCoverage().toMap
      assert(c1("cover0") == 5)
      assert(c1("cover1") == 3)
      assert(c1("cover2") == 2)
      assert(c1("cover3") == 1)
      assert(c1("c.cover0") + c1("c.cover1") == 10)
    }
  }


  "cover statements should produce a report" in {
    // report will go in coverageFileName so delete it if it already exists
    val coverageFileName = "test_run_dir/HasCoverStatements/HasCoverStatements.coverage.txt"
    if (new File(coverageFileName).exists()) {
      new File(coverageFileName).delete()
    }

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      tester.step(10)
    }
    new File(coverageFileName).exists() should be(true)

    val lines = FileUtils.getLines(coverageFileName)
    val expectedLines = Seq(
      ""","cov0",10,2""",
      ""","cov1",10,8""",
      """@[VerificationSpec.scala 42:19],"register 0 cover",10,5""",
      """@[VerificationSpec.scala 52:19],"register 1 cover",5,3""",
      """@[VerificationSpec.scala 62:19],"register 2 cover",3,2""",
      """@[VerificationSpec.scala 72:19],"register 3 cover",2,1"""
    )
    lines.zip(expectedLines).foreach { case (a, b) =>
      a should be(b)
    }
  }

  "pretty printer provided can show coverage on firrtl source" in {
    val firrtlFileName = "src/test/resources/HasCoverStatements.fir"
    val coverageFileName = "test_run_dir/HasCoverStatements/HasCoverStatements.coverage.txt"
    if (new File(coverageFileName).exists()) {
      new File(coverageFileName).delete()
    }

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(firrtlSource))) { tester =>
      tester.step(10)
    }
    new File(coverageFileName).exists() should be(true)

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      CoveragePrettyPrinterMain.main(
        Array(
          "--cpp-firrtl-file",
          firrtlFileName,
          "--cpp-coverage-file",
          coverageFileName
        )
      )
    }
    val outputLines = outputBuffer.toString

    val expectedLines = Seq(
      """cover(clock, out_reg, UInt<1>("h1"), "register 0 cover")  @[VerificationSpec.scala 42:19]   COV(10,5)""",
      """cover(_out_T, out_reg_1, UInt<1>("h1"), "register 1 cover")  @[VerificationSpec.scala 52:19]   COV(5,3)""",
      """cover(_out_T_1, out_reg_2, UInt<1>("h1"), "register 2 cover")  @[VerificationSpec.scala 62:19]   COV(3,2)""",
      """cover(_out_T_2, out_reg_3, UInt<1>("h1"), "register 3 cover")  @[VerificationSpec.scala 72:19]   COV(2,1)"""
    )
    expectedLines.foreach { expectedLine =>
      outputLines should include(expectedLine)
    }
  }
}
