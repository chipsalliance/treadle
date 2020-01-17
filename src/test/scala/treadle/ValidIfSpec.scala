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

//
//package treadle
//
//import firrtl.stage.FirrtlSourceAnnotation
//import org.scalatest.{FreeSpec, Matchers}
//
//
////scalastyle:off magic.number
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
//    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
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
//    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), ValidIfIsRandomAnnotation))
//    tester.poke("in1", 42)
//    assert(tester.peek("out1") != 42)
//  }
//}
