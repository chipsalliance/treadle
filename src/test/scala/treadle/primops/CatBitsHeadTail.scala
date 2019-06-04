// See LICENSE for license details.

package treadle.primops

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.{BitTwiddlingUtils, _}
import treadle.executable._


// scalastyle:off magic.number
class CatBitsHeadTail extends FreeSpec with Matchers {
  def f0(): Int = 0
  def f1(): Int = 1
  def f2(): Int = 2
  def f3(): Int = 3
  def fMinus1(): Int = -1
  def fMinus2(): Int = -2
  def fMinus3(): Int = -3
  def fMinus4(): Int = -4
  def fMinus6(): Int = -6

  def val1(): Int = Integer.parseInt("abcd", 16)
  def val2(): Int = Integer.parseInt("10" * 4, 2)
  def val3(): Int = Integer.parseInt("0", 2)

  "Cat Bits Head and Tail should pass basic tests" - {
    "Cat should pass the following tests" - {
      def doCatCheck(num1: Big, width1: Int, num2: Big, width2: Int): Unit = {
        val got = (
          CatInts(()  => num1.toInt,  width1, () => num2.toInt,  width2).apply(),
          CatLongs(() => num1.toLong, width1, () => num2.toLong, width2).apply(),
          CatBigs(()  => num1,        width1, () => num2,        width2).apply()
        )
        val expected = (
          BitTwiddlingUtils.cat(num1, width1, num2, width2),
          BitTwiddlingUtils.cat(num1, width1, num2, width2),
          BitTwiddlingUtils.cat(num1, width1, num2, width2)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of sints" in {
        for {
          bitWidth1 <- 1 to 4
          bitWidth2 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          val (lo2, hi2) = extremaOfSIntOfWidth(bitWidth2)
          for {
            num1 <- lo1 to hi1
            num2 <- lo2 to hi2
          } {
            doCatCheck(num1, bitWidth1, num2, bitWidth2)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
          bitWidth2 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          val (lo2, hi2) = extremaOfUIntOfWidth(bitWidth2)
          for {
            num1 <- lo1 to hi1
            num2 <- lo2 to hi2
          } {
            doCatCheck(num1, bitWidth1, num2, bitWidth2)
          }
        }
      }

      "sign extension should not happen" in {
        val input =
          """
            |circuit CatProblem :
            |  module CatProblem :
            |    input clock : Clock
            |    output out : UInt<160>
            |
            |    node _T_310 = cat(UInt<32>("hffdff06f"), UInt<32>("h73")) @[Cat.scala 30:58]
            |    node _T_311 = cat(UInt<32>("h0"), UInt<32>("h0")) @[Cat.scala 30:58]
            |    node _T_312 = cat(_T_311, UInt<32>("h0")) @[Cat.scala 30:58]
            |    node _T_313 = cat(_T_312, _T_310) @[Cat.scala 30:58]
            |
            |    out <= _T_313
            |
          """.stripMargin

        val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
        println(s"peek out 0x${tester.peek("out").toString(16)}")
        tester.report()
      }

    }

    "Bits should bass the following tests" - {
      def doBitsCheck(i: Big, hi: Int, lo: Int, bitWidth: Int): Unit = {
        val got = (
          BitsInts(() => i.toInt,   hi, lo, originalWidth = bitWidth).apply(),
          BitsLongs(() => i.toLong, hi, lo, originalWidth = bitWidth).apply(),
          BitsBigs(() => i,         hi, lo, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth),
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth),
          BitTwiddlingUtils.bits(i, hi, lo, bitWidth)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "bits should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for {
          i     <- lo to hi
          loBit <- 0 until bitWidth
          hiBit <- loBit until bitWidth
        } {
          doBitsCheck(i, hiBit, loBit, bitWidth)
        }
      }
      "bits should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for {
          i     <- lo to hi
          loBit <- 0 until bitWidth
          hiBit <- loBit until bitWidth
        } {
          doBitsCheck(i, hiBit, loBit, bitWidth)
        }
      }
    }

    "Head should bass the following tests" - {
      def doHeadCheck(i: Big, takeBits: Int, bitWidth: Int): Unit = {
        val got = (
          HeadInts(() => i.toInt,   takeBits, originalWidth = bitWidth).apply(),
          HeadLongs(() => i.toLong, takeBits, originalWidth = bitWidth).apply(),
          HeadBigs(() => i,         takeBits, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "head should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
      "head should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
    }

    "Tail should pass following tests" - {
      def doTailCheck(i: Big, bitWidth: Int): Unit = {
        val got = (
          TailInts(() => i.toInt, toDrop = 1, originalWidth = bitWidth).apply(),
          TailLongs(() => i.toLong, toDrop = 1, originalWidth = bitWidth).apply(),
          TailBigs(() => i, toDrop = 1, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "tail should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail ops should drop leading bits from expression" in {
        TailInts(() => -22, toDrop = 1, originalWidth = 16)() should be(32746)

        TailInts(f1, toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(f2, toDrop = 1, originalWidth = 3)() should be(2)
        TailInts(f3, toDrop = 1, originalWidth = 3)() should be(3)
        TailInts(f3, toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(fMinus3, toDrop = 1, originalWidth = 4)() should be(5)
        TailInts(fMinus4, toDrop = 1, originalWidth = 4)() should be(4)

        val tailOps = TailInts(val1, toDrop = 9, originalWidth = 17)
        // println(f"TailInts(${val1()}%x, toDrop = 8) -> ${tailOps()}%x")
        tailOps() should be(Integer.parseInt("cd", 16))
      }
    }
  }
}
