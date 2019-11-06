// See README.md for license details.

package treadle

import firrtl.annotations.DeletedAnnotation
import firrtl.options.TargetDirAnnotation
import firrtl.{AnnotationSeq, HighFirrtlCompiler, HighForm, LowFirrtlCompiler, LowForm, MidForm, MiddleFirrtlCompiler}
import firrtl.stage.{CompilerAnnotation, FirrtlCircuitAnnotation, FirrtlSourceAnnotation, FirrtlStage}
import org.scalatest.{FreeSpec, Matchers}
import treadle.stage.TreadleTesterPhase

class TesterCreationTest extends FreeSpec with Matchers {
  "TreadleTester can be created from any form of firrtl" - {
    //scalastyle:off method.length
    def testWithCompiler(compilerAnnotation: CompilerAnnotation): Unit = {
      val readLatency = 1
      val writeLatency = 1
      val input =
        s"""circuit Test :
           |  module Test :
           |    input clock    : Clock
           |    input in1      : UInt<8>
           |    input addr     : UInt<8>
           |    input write_en : UInt<1>
           |    output out1    : UInt<8>
           |    mem m :
           |      data-type => UInt<8>
           |      depth => 32
           |      read-latency => $readLatency
           |      write-latency => $writeLatency
           |      reader => read
           |      writer => write
           |
           |    m.read.clk <= clock
           |    m.read.en <= eq(write_en, UInt<1>(0))
           |    m.read.addr <= addr
           |
           |    m.write.clk <= clock
           |    m.write.en <= eq(write_en, UInt<1>(1))
           |    m.write.mask <= UInt<8>("hff")
           |    m.write.addr <= addr
           |    m.write.data <= in1
           |
           |    out1 <= m.read.data
      """.stripMargin


      var annos: AnnotationSeq = Seq(
        TargetDirAnnotation("test_run_dir/tester_creation"),
        FirrtlSourceAnnotation(input),
        compilerAnnotation
      )

      val formHint = TreadleFirrtlFormHint(compilerAnnotation.compiler match {
        case c: HighFirrtlCompiler   => HighForm
        case c: MiddleFirrtlCompiler => MidForm
        case c: LowFirrtlCompiler    => LowForm
      })

      annos = (new FirrtlStage).run(annos)

      println(
        s"Post compiler Annotations\n" +
          annos
//            .filterNot(_.isInstanceOf[DeletedAnnotation])
            .map(_.toString.split("\n"))
            .map { l => l.head + (if(l.length > 1 ) "\n" + l.last else "") }
            .mkString("\n")
      )

      annos.exists(_.isInstanceOf[FirrtlCircuitAnnotation]) should be(true)
      annos.exists(_.isInstanceOf[FirrtlSourceAnnotation]) should be(false)

      annos = TreadleTesterPhase.transform(annos :+ formHint)
      annos.exists(_.isInstanceOf[TreadleTesterAnnotation]) should be(true)

      println(
        s"Post Creation Annotations\n" +
          annos
          .filterNot(_.isInstanceOf[DeletedAnnotation])
          .map(_.toString.split("\n"))
          .map { l => l.head + (if(l.length > 1 ) "\n" + l.last else "") }
          .mkString("\n")
      )
    }

    "Create tester from firrtl circuit with HighFirrtlCompiler" in {
      testWithCompiler(CompilerAnnotation(new HighFirrtlCompiler))
    }

    "Create tester from firrtl circuit with MiddleFirrtlCompiler" in {
      testWithCompiler(CompilerAnnotation(new MiddleFirrtlCompiler))
    }

    "Create tester from firrtl circuit with LowFirrtlCompiler" in {
      testWithCompiler(CompilerAnnotation(new LowFirrtlCompiler))
    }
  }
}
