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

package treadle

import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number
class TestUtilsSpec extends FlatSpec with Matchers {
  behavior.of("IntGenerator")

  it should "return a bunch of ints" in {
    val l = for (i <- IntWidthTestValuesGenerator(8, 256)) yield i
    val expected = List(8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256)
//    println(l.mkString(","))
    l.zip(expected.iterator).foreach {
      case (l1, e1) =>
        l1 should be(e1)
    }
  }
  it should "return a allow negative  to positive range" in {

    val l = for (i <- IntWidthTestValuesGenerator(-256, 256)) yield i
    val expected = List(-256, -255, -129, -128, -127, -65, -64, -63, -33, -32, -31, -17, -16, -15, -9, -8, -7, -5, -4,
      -3, -2, -1, 0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256)
//    println(l.mkString(","))
    l.zip(expected.iterator).foreach {
      case (l1, e1) =>
        l1 should be(e1)
    }
  }

  it should "return a allow negative to negative range" in {
    var count = 0
    for (i <- IntWidthTestValuesGenerator(-18, -12)) {
      print(s"$i")
      if (count > 18) assert(false, s"count $count is bigger than 18")
      count += 1
    }
    println()
  }

  it should "work with following specific examples" in {
    println(IntWidthTestValuesGenerator(-33, 6).toList.mkString(","))
    println(IntWidthTestValuesGenerator(1, 10).toList.mkString(","))
    println(IntWidthTestValuesGenerator(1, 5).toList.mkString(","))
  }

  it should "never repeat a value" in {
    for (i <- -TestUtils.MaxTestingWidth to TestUtils.MaxTestingWidth) {
      for (j <- i to TestUtils.MaxTestingWidth) {
        val iterator = IntWidthTestValuesGenerator(i, j)
        var lastSeen = 0
        if (iterator.hasNext()) {
          lastSeen = iterator.next
          lastSeen should be(i)
          for (k <- iterator) {
            k should be > lastSeen
            lastSeen = k
          }
          lastSeen should be(j)
        }
      }
    }
  }

  behavior.of("BigIntGenerator")

  it should "return a bunch of BigInts" in {
    //    val gen = IntGenerator(8, 256)
    //    gen.foreach { case i =>
    //      println(s"got $i")
    //    }

    val l = for (i <- BigIntTestValuesGenerator(8, 256)) yield i
    val expected = List(8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256)
//    println(l.mkString(","))
    l.zip(expected.iterator).foreach {
      case (l1, e1) =>
        l1 should be(e1)
    }
  }
  it should "return a allow negative  to positive range" in {
    //    var count = 0
    //    for (i <- BigIntGenerator(-8, 8)) {
    //      println(s"$i")
    //      if(count > 18) assert(false)
    //      count += 1
    //
    //    }
    val l = for (i <- BigIntTestValuesGenerator(-256, 256)) yield i
    val expected = List(-256, -255, -129, -128, -127, -65, -64, -63, -33, -32, -31, -17, -16, -15, -9, -8, -7, -5, -4,
      -3, -2, -1, 0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256)
//    println(l.mkString(","))
    l.zip(expected.iterator).foreach {
      case (l1, e1) =>
        l1 should be(e1)
    }

  }

  it should "return a allow negative to negative range" in {
    var count = 0
    for (i <- BigIntTestValuesGenerator(-18, -12)) {
//      println(s"$i")
      if (count > 18) assert(false, s"count $count is bigger than 18")
      count += 1

    }
  }

  it should "work with very big numbers" in {
    //    for(i <- BigIntGenerator(-BigInt("1"*10, 2), BigInt("1"*10, 2))) {
    //    for(i <- BigIntGenerator(-8, BigInt("1"*100, 2))) {
    for (i <- BigIntTestValuesGenerator(-BigInt("1" * 100, 2), 8)) {
//      println(s"$i")
    }
  }

  it should "work with very big numbers by specifying widths" in {
    def testGenerator(startWidth: Int, finishWidth: Int): Unit = {
      for (i <- BigIntTestValuesGenerator.fromWidths(startWidth, finishWidth)) {
//        print(s"$i ")
      }
//      println()
    }

    testGenerator(44, 60)

    val stepSize = 8
    for {
      startWidth <- -TestUtils.MaxTestingWidth to TestUtils.MaxTestingWidth by stepSize
      finishWidth <- startWidth + stepSize to TestUtils.MaxTestingWidth by stepSize
      if startWidth != 0
      if finishWidth != 0
    } {
//      println(s"New generator for $startWidth to $finishWidth")
      testGenerator(startWidth, finishWidth)
    }
  }

  it should "never repeat a value" in {
    for (i <- -TestUtils.MaxTestingWidth to TestUtils.MaxTestingWidth) {
      for (j <- i to TestUtils.MaxTestingWidth) {
        val iterator = BigIntTestValuesGenerator.fromWidths(i, j)
        var lastSeen = Big0
        if (iterator.hasNext()) {
          lastSeen = iterator.next
          lastSeen should be(TestUtils.powerOfTwoFrom(i))
          for (k <- iterator) {
            k should be > lastSeen
            lastSeen = k
          }
          lastSeen should be(TestUtils.powerOfTwoFrom(j))
        }
      }
    }
  }
}
