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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle.executable.IndicesAndRadix
import treadle.utils.NumberHelpers._

class NumberHelpersSpec extends AnyFreeSpec with Matchers {
  "NumberHelpers" - {
    "Can parse a radix code" in {
      radixFromString("b") must be(2)
      radixFromString("o") must be(8)
      radixFromString("d") must be(10)
      radixFromString("x") must be(16)
      radixFromString("h") must be(16)
      radixFromString("anything else") must be(10)
    }

    "parse string with radix and range of indices" in {
      IndicesAndRadix(Set.empty).radix must be(10)
      parseIntRangeWithRadix("10,20") must be(IndicesAndRadix(Set(10, 20)))
      parseIntRangeWithRadix("o10,o20") must be(IndicesAndRadix(Set(8, 16), 8))
      parseIntRangeWithRadix("0x10-0x20") must be(IndicesAndRadix(Seq.tabulate(17)(_ + 16).toSet, 16))
    }

    "parse bigint strings with leading radix string" in {
      parseBigInt("b111") must be(BigInt(7))
      parseBigInt("o377") must be(BigInt(255))
      parseBigInt("hff") must be(BigInt(255))
      parseBigInt("hff") must be(BigInt(255))
      parseBigInt("xff") must be(BigInt(255))
      parseBigInt("0xff") must be(BigInt(255))
      parseBigInt("0x-ff") must be(BigInt(-255))
    }

    "parse bigint string with radix and return value and radix" in {
      parseBigIntWithRadix("b111") must be(BigInt(7), 2)
      parseBigIntWithRadix("377") must be(BigInt(377), 10)
      parseBigIntWithRadix("o377") must be(BigInt(255), 8)
      parseBigIntWithRadix("hff") must be(BigInt(255), 16)
      parseBigIntWithRadix("hff") must be(BigInt(255), 16)
      parseBigIntWithRadix("xff") must be(BigInt(255), 16)
      parseBigIntWithRadix("0xff") must be(BigInt(255), 16)
      parseBigIntWithRadix("0x-ff") must be(BigInt(-255), 16)
    }
  }
}
