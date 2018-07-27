// See LICENSE for license details.

package treadle

import org.scalatest.{Matchers, FreeSpec}

class DriverSpec extends FreeSpec with Matchers {
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
      //      val engine = Driver.execute(Array.empty[String], input)
//      val engine = Driver.execute(Array(), input)

      val result = Driver.execute(Array.empty, Seq(TreadleFirrtlString(input)))

      result.isInstanceOf[TreadleTesterCreated] should be (true)

      val tester = result.asInstanceOf[TreadleTesterCreated].treadleTester

      tester.poke("x", 1)
      tester.expect("y", 1)
    }
  }
}
