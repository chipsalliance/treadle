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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DriverSpec extends AnyFreeSpec with Matchers {
  "The Driver class provides a simple caller with run-time parameters" - {
    "topName must be set" in {
      val input =
        """
          |circuit Dummy :
          |  module Dummy :
          |    input x : UInt<1>
          |    output y : UInt<1>
          |    y <= x
        """.stripMargin

      val engine = Driver.execute(Array("--tr-verbose"), input)

      engine should not be empty

      engine.foreach { tester =>
        tester.poke("x", 1)
        tester.expect("y", 1)
      }
    }
  }
}
