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

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.executable.{ClockInfo, StopException}

//scalastyle:off magic.number
class RiscVMiniSimpleSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "riscv-mini simple core test should run then stop" in {

    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input =
      scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val output = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(output)) {

      TreadleTestHarness(
        Seq(
          FirrtlSourceAnnotation(input),
          ClockInfoAnnotation(
            Seq(ClockInfo("clock", period = 10, initialOffset = 1))
          )
        )
      ) { tester =>

        intercept[StopException] {
          tester.step(400)
        }
        tester.engine.lastStopResult should be(Some(0))
      }
    }
    logger.debug(output.toString)
  }
}
