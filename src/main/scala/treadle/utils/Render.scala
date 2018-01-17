// See LICENSE for license details.

package treadle.utils

object Render {
  def binary(value: BigInt, bitWidth: Int): String = {
    val numberString = if(value < 0) {
      val powerOfTwo = BitMasks.getBitMasksBigs(bitWidth).nextPowerOfTwo
      val positiveValue = value + powerOfTwo
      positiveValue.toString(2)
    }
    else {
      value.toString(2)
    }
    if(bitWidth > numberString.length) {
      ("0" * (bitWidth - numberString.length)) + numberString
    }
    else {
      numberString
    }
  }
}
