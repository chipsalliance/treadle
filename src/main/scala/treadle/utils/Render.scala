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

package treadle.utils

//scalastyle:off magic.number regex
object Render {
  def binary(value: BigInt, bitWidth: Int): String = {
    val numberString = if (value < 0) {
      val powerOfTwo = BitMasks.getBitMasksBigs(bitWidth).nextPowerOfTwo
      val positiveValue = value + powerOfTwo
      positiveValue.toString(2)
    } else {
      value.toString(2)
    }
    if (bitWidth > numberString.length) {
      ("0" * (bitWidth - numberString.length)) + numberString
    } else {
      numberString
    }
  }

  def headerBar(string: String, offset: Int = 4, width: Int = 80): Unit = {
    val header = "-" * offset + " " + string + " " +
      ("-" * (width - string.length - offset - 2))
    println(header)
  }
}
