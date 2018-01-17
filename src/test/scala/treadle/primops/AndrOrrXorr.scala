// See LICENSE for license details.

package treadle.primops

import treadle.executable._
import treadle.{BigIntTestValuesGenerator, BitTwiddlingUtils, extremaOfUIntOfWidth, extremaOfSIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class AndrOrrXorr extends FreeSpec with Matchers {
  "BitReductions should pass a basic test" - {
    "And reduction (Andr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false).toInt
        AndrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "And reduction (Andr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true).toInt
        AndrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true).toInt
        // println(s"input $input ${(input + 1024).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, bitWidth).apply() should be(expected)
      }
    }

    "Reductions should pass for different bit widths when using UInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, bitWidth).apply()),
                Big(OrrInts(()  => i.toInt, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, bitWidth).apply()),
                Big(OrrLongs(()  => i.toLong, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, bitWidth).apply(),
                OrrBigs(() => i,  bitWidth).apply(),
                XorrBigs(() => i, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false)
          )

          // println(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult  should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }

    "Reductions should pass for different bit widths when using SInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, bitWidth).apply()),
                Big(OrrInts(()  => i.toInt, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, bitWidth).apply()),
                Big(OrrLongs(()  => i.toLong, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, bitWidth).apply(),
                OrrBigs(() => i,  bitWidth).apply(),
                XorrBigs(() => i, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true)
          )

//          println(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult  should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }
  }
}
