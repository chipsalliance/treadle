// SPDX-License-Identifier: Apache-2.0

package treadle.primops

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.executable._
import treadle.utils.Render
import treadle.{BitTwiddlingUtils, TreadleTestHarness, extremaOfSIntOfWidth, extremaOfUIntOfWidth}

// scalastyle:off magic.number
class ShlShrDshlDshr extends AnyFreeSpec with Matchers with LazyLogging {
  "Shl should work with known examples" - {
    "Using SInts" in {
      val bitWidth = 4
      // val outBitWidthMax = bitWidth * 3
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j.toInt
        val staticShifter = ShlInts(() => a, () => b).apply _
        val dynamicShifter = DshlInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shl(a, b).toInt

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }

    "dynamic shift with constant" in {
      val input =
        """
          |circuit ShiftTest :
          |  module ShiftTest :
          |    input clk : Clock
          |    input reset : UInt<1>
          |    input  intInput    : UInt<30>
          |    input  longInput   : UInt<60>
          |
          |    output intToLong    : UInt<90>
          |    output intToBig     : UInt<128>
          |
          |    output longToBig    : UInt<90>
          |
          |    intToLong    <= dshl(intInput, UInt<7>(32))
          |    intToBig     <= dshl(intInput, UInt<7>(60))
          |
          |    longToBig    <= dshl(longInput, UInt<7>(32))

      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
        val intInput = BigInt("1234", 16)
        val longInput = BigInt("123456789abc", 16)
        t.poke("intInput", intInput)
        t.poke("longInput", longInput)

        val intToLong: BigInt = t.peek("intToLong")
        val intToBig = t.peek("intToBig")
        val longToBig = t.peek("longToBig")

        logger.debug(
          f"intInput  ${intInput.toString(16)} << 32 yields long ${intToLong.toString(16)} with ${intToLong.bitLength}"
        )
        logger.debug(
          f"intInput  ${intInput.toString(16)} << 60 yields big  ${intToBig.toString(16)}  with ${intToBig.bitLength}"
        )
        logger.debug(
          f"longInput ${intInput.toString(16)} << 30 yields big  ${longToBig.toString(16)} with ${longToBig.bitLength}"
        )

        t.expect("intToLong", BigInt("123400000000", 16))
        t.expect("intToBig", BigInt("1234000000000000000", 16))
        t.expect("longToBig", BigInt("123456789abc00000000", 16))
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j.toInt
        val staticShifter = ShlInts(() => a, () => b).apply _
        val dynamicShifter = DshlInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shl(a, b).toInt

        logger.debug(f"inputs $a%5d (${Render.binary(a, 4)})" +
          f" << $b%5d " +
          f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})")

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }
  }

  "Shr should work with known examples" - {
    "Using SInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j.toInt
        val staticShifter = ShrInts(() => a, () => b).apply _
        val dynamicShifter = DshrInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shr(a, b).toInt

        logger.debug(f"inputs $a%5d (${Render.binary(a, 4)})" +
          f" >> $b%5d " +
          f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})")

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for {
        i <- lo to hi
        j <- 0 to bitWidth * 2
      } {
        val a = i.toInt
        val b = j.toInt
        val staticShifter = ShrInts(() => a, () => b).apply _
        val dynamicShifter = DshrInts(() => a, () => b).apply _
        val staticExpected = BitTwiddlingUtils.shr(a, b).toInt

        logger.debug(f"inputs $a%5d (${Render.binary(a, 4)})" +
          f" >> $b%5d " +
          f" $staticExpected%5d (${Render.binary(staticExpected, bitWidth)})")

        staticShifter() should be(staticExpected)
        dynamicShifter() should be(staticExpected)
      }
    }
  }
}
