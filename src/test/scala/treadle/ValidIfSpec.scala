// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
//class ValidIfSpec extends FreeSpec with Matchers {
//  "ValidIf should return expression when not random" in {
//    val input =
//      """
//        |circuit ValidIfExample :
//        |  module ValidIfExample :
//        |    input in1    : UInt<16>
//        |    input enable : UInt<1>
//        |    output out1  : UInt<16>
//        |    out1 <= validif(enable, in1)
//      """.stripMargin
//
//    val options = new InterpreterOptionsManager {
//      treadleOptions = treadleOptions.copy(validIfIsRandom = false)
//    }
//    val tester = new TreadleTester(input, options)
//    tester.poke("in1", 42)
//    tester.expect("out1", 42)
//  }
//  "ValidIf should not return expression when not set to random" in {
//    val input =
//      """
//        |circuit ValidIfExample :
//        |  module ValidIfExample :
//        |    input in1    : UInt<16>
//        |    input enable : UInt<1>
//        |    output out1  : UInt<16>
//        |    out1 <= validif(enable, in1)
//      """.stripMargin
//
//    val options = new InterpreterOptionsManager {
//      treadleOptions = treadleOptions.copy(
//        validIfIsRandom = true,
//        randomSeed = 0L
//      )
//    }
//    val tester = new TreadleTester(input, options)
//    tester.poke("in1", 42)
//    assert(tester.peek("out1") != 42)
//  }
//}
