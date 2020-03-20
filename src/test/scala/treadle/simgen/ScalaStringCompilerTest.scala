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

////////////////////////////////////////////////////////////////////////////////////
// THIS IS EXPERIMENTAL. NOT INTENDED OR READY FOR PRODUCTION USE
////////////////////////////////////////////////////////////////////////////////////

package treadle.simgen

import java.io.{ByteArrayOutputStream, PrintStream}

import org.scalatest.{FreeSpec, Matchers}

class ScalaStringCompilerTest extends FreeSpec with Matchers {
  "A simple example compiling multiple classes and running" in {
    val input =
      """
        |import treadle.simgen.CompileTreadleRunner
        |
        |class CT2 {
        |  val a = Array.fill(10) { 0 }
        |}
        |
        |class CT3(ct2: CT2) {
        |  def run: Unit = {
        |    ct2.a(0) = 10
        |    ct2.a(1) = 20
        |    ct2.a(2) = ct2.a(0) + ct2.a(1)
        |    println(s"Result:\n" + ct2.a.mkString(", "))
        |  }
        |}
        |
        |class CTMain extends CompileTreadleRunner {
        |  def run: Unit = {
        |    val ct2 = new CT2
        |    val ct3 = new CT3(ct2)
        |    ct3.run
        |  }
        |}
        |
        |new CTMain
        |
        |""".stripMargin

    val runner = ScalaStringCompiler(input)

    val outputBuffer = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputBuffer)) {
      runner.run()
    }
    outputBuffer.toString.contains("10, 20, 30, 0, 0, 0, 0, 0, 0, 0") should be (true)
  }
}
