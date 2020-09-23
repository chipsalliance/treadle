// SPDX-License-Identifier: Apache-2.0

package treadle.primops

import firrtl.stage.FirrtlSourceAnnotation
import treadle.executable.{AndInts, OrInts, XorInts}
import treadle.{extremaOfSIntOfWidth, extremaOfUIntOfWidth, BitTwiddlingUtils, TreadleTester}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class AndOrXor extends AnyFreeSpec with Matchers {
  "And should work with simple bit width" - {

    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = AndInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" & $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = AndInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" & $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }
  }

  "Or should work with simple bit width" - {
    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = OrInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.or(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" | $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = OrInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.or(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" | $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }
  }

  "Xor should work with simple bit width" - {
    "using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = XorInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.xor(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" ^ $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }

    "using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- lo to hi
      } {
        val a = i.toInt
        val b = j.toInt
        val primpOp = XorInts(() => a, () => b, bitWidth).apply _
        val expected = BitTwiddlingUtils.xor(a, b, bitWidth).toInt

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" ^ $b%5d (${Render.binary(b, 4)})" +
        //   f" $expected%5d (${Render.binary(expected, bitWidth)})")

        primpOp() should be(expected)
      }
    }
  }

  "And output is a uint" in {
    val input =
      """
        |circuit Ander :
        |  module Ander :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input a : SInt<4>
        |    input b : SInt<4>
        |    output c : UInt<4>
        |    c <= and(a, b)
        |
      """.stripMargin
    val bitWidth = 4
    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

    val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
    for {
      a <- lo to hi
      b <- lo to hi
    } {
      tester.poke("a", a)
      tester.poke("b", b)
      val expected = BitTwiddlingUtils.and(a, b, bitWidth)
      // println(s"and test $a & $b => ${tester.peek("c")}")
      tester.expect("c", expected)
    }
    tester.report()
  }

  "And should work with known examples of UInts" in {
    val bitWidth = 4
    val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
    for {
      i <- lo to hi
      j <- lo to hi
    } {
      val a = i.toInt
      val b = j.toInt
      val expected = BitTwiddlingUtils.and(a, b, bitWidth).toInt
      // println(f"inputs $a%5d (${(a + 32).toBinaryString.takeRight(4)})" +
      //  f" $b%5d (${(b + 32).toBinaryString.takeRight(4)})" +
      //  f" $expected%5d (${(expected + 32).toBinaryString.takeRight(4)})")

      AndInts(() => a, () => b, bitWidth).apply() should be(expected)
    }
  }
}
