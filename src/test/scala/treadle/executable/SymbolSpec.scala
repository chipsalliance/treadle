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

package treadle.executable

import firrtl.RegKind
import firrtl.ir.{IntWidth, NoInfo, SIntType}
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable
import scala.util.Random

class SymbolSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "symbol information can be used to generate random values for SInt that will fill the range" in {
    val s = Symbol("bob", SIntType(IntWidth(4)), RegKind, 1, NoInfo)

    val rand = new Random
    rand.setSeed(s.name.hashCode + 10)
    val set = new mutable.HashSet[BigInt]
    for (_ <- 0 to 1000) {
      val randomBig = BigInt(4, rand)
      val value = s.makeSInt(randomBig, 4)
      set += value
    }
    logger.debug(s"value = ${set.toSeq.sorted.mkString(",")}")
    (BigInt(-8) to BigInt(7)).forall(x => set.contains(x)) must be(true)
  }
}
