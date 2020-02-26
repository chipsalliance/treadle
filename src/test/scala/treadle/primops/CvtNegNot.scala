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

package treadle.primops

import treadle.executable._
import treadle.{extremaOfSIntOfWidth, extremaOfUIntOfWidth, BitTwiddlingUtils}
import org.scalatest.{FreeSpec, Matchers}

// scalastyle:off magic.number
class CvtNegNot extends FreeSpec with Matchers {
  "Cvt, Neg and Not should pass a basic test" - {
    "Cvt is a no-op for the simulator" in {
      true should be(true)
    }

    "Neg should pass the following tests" - {
      def doNegCheck(num1: Big): Unit = {
        val got = (
          NegInts(() => num1.toInt).apply(),
          NegLongs(() => num1.toLong).apply(),
          NegBigs(() => num1).apply()
        )
        val expected = (
          BitTwiddlingUtils.neg(num1),
          BitTwiddlingUtils.neg(num1),
          BitTwiddlingUtils.neg(num1)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of SInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNegCheck(num1)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNegCheck(num1)
          }
        }
      }

    }

    "Not should pass the following tests" - {
      def doNotCheck(num1: Big, width: Int): Unit = {
        val got = (
          NotInts(() => num1.toInt, width).apply(),
          NotLongs(() => num1.toLong, width).apply(),
          NotBigs(() => num1, width).apply()
        )
        val expected = (
          BitTwiddlingUtils.not(num1, width),
          BitTwiddlingUtils.not(num1, width),
          BitTwiddlingUtils.not(num1, width)
        )

        println(s"i $num1 got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of SInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNotCheck(num1, bitWidth1)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNotCheck(num1, bitWidth1)
          }
        }
      }

    }
  }
}
