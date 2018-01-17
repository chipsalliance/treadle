// See LICENSE for license details.

package treadle.primops

import treadle.executable._
import treadle.{BitTwiddlingUtils, extremaOfSIntOfWidth, extremaOfUIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class ShlShrDshlDshr extends FreeSpec with Matchers {
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

        //  println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //    f" << $b%5d " +
        //    f" $staticExpected%5d (${Render.binary(staticExpected, outBitWidthMax)})")

        staticShifter() should be (staticExpected)
        dynamicShifter() should be (staticExpected)
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      // val outBitWidthMax = bitWidth * 3
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

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" << $b%5d " +
        //   f" $staticExpected%5d (${Render.binary(staticExpected, outBitWidthMax)})")

        staticShifter() should be (staticExpected)
        dynamicShifter() should be (staticExpected)
      }
    }
  }

  "Shr should work with known examples" - {
    "Using SInts" in {
      val bitWidth = 4
      // val outBitWidthMax = bitWidth
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

        //  println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //    f" >> $b%5d " +
        //    f" $staticExpected%5d (${Render.binary(staticExpected, outBitWidthMax)})")

        staticShifter() should be (staticExpected)
        dynamicShifter() should be (staticExpected)
      }
    }

    "Using UInts" in {
      val bitWidth = 4
      // val outBitWidthMax = bitWidth
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

        // println(f"inputs $a%5d (${Render.binary(a, 4)})" +
        //   f" >> $b%5d " +
        //   f" $staticExpected%5d (${Render.binary(staticExpected, outBitWidthMax)})")

        staticShifter() should be (staticExpected)
        dynamicShifter() should be (staticExpected)
      }
    }
  }
}
