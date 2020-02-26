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

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class DeepExecutionSpec extends FreeSpec with Matchers {
  "DeepExecutionSpec should pass a basic test" in {
    val stream = getClass.getResourceAsStream("/DeepExecution.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    tester.step(100)
    tester.report()
  }
}
