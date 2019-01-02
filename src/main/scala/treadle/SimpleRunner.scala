// See LICENSE for license details.

package treadle

import treadle.Regression.computeGcd



class GCDScalaImpl {
  val intArray = Array.fill(20)(0)
  val longArray = Array.fill(0)(0L)
  val bigArray = Array.fill(0)(BigInt(0))


  val `GEN_1` = 0
  val `T_19` = 1
  val `io_e` = 2
  val `io_z` = 3
  val `GEN_0` = 4
  val `T_15` = 5
  val `y/in` = 6
  val `T_18` = 7
  val `io_b` = 8
  val `y` = 9
  val `clock` = 10
  val `T_17` = 11
  val `x` = 12
  val `io_v` = 13
  val `T_14` = 14
  val `io_a` = 15
  val `reset` = 16
  val `x/in` = 17
  val `clock/prev` = 18
  val `T_13` = 19


  val dataIndices = Map(
    "GEN_1" -> 0,
    "T_19" -> 1,
    "io_e" -> 2,
    "io_z" -> 3,
    "GEN_0" -> 4,
    "T_15" -> 5,
    "y/in" -> 6,
    "T_18" -> 7,
    "io_b" -> 8,
    "y" -> 9,
    "clock" -> 10,
    "T_17" -> 11,
    "x" -> 12,
    "io_v" -> 13,
    "T_14" -> 14,
    "io_a" -> 15,
    "reset" -> 16,
    "x/in" -> 17,
    "clock/prev" -> 18,
    "T_13" -> 19  )

  def poke(s: String, value: Int): Unit = {
    intArray(dataIndices(s)) = value
  }

  def peek(s: String): Int = {
    intArray(dataIndices(s))
  }

  var cycles = 0

  def step(steps: Int): Unit = {
    var step = 0
    while(step < steps) {
      update()
      intArray(dataIndices("clock")) = 1
      update()
      intArray(dataIndices("clock")) = 0
      step += 1
    }
  }


  def update(): Unit = {

    if(intArray(`clock`) > 0 && intArray(`clock/prev`) == 0) {
      intArray(`y`) = intArray(`y/in`)
    }


    intArray(`io_v`) = if((intArray(`y`)) == (0)) { 1 } else { 0 }


    if(intArray(`clock`) > 0 && intArray(`clock/prev`) == 0) {
      intArray(`x`) = intArray(`x/in`)
    }


    intArray(`io_z`) = intArray(`x`)

    intArray(`T_18`) = (intArray(`y`)) - (intArray(`x`))
    intArray(`T_19`) = (intArray(`T_18`)) & 1048575
    intArray(`T_14`) = (intArray(`x`)) - (intArray(`y`))
    intArray(`T_15`) = (intArray(`T_14`)) & 1048575
    intArray(`T_13`) = if((intArray(`x`)) > (intArray(`y`))) { 1 } else { 0 }
    intArray(`GEN_0`) = if(intArray(`T_13`) > 0) { intArray(`T_15`) } else { intArray(`x`) }

    intArray(`T_17`) = if((intArray(`T_13`)) == (0)) { 1 } else { 0 }
    intArray(`GEN_1`) = if(intArray(`T_17`) > 0) { intArray(`T_19`) } else { intArray(`y`) }


    intArray(`y/in`) = if(intArray(`io_e`) > 0) { intArray(`io_b`) } else { intArray(`GEN_1`) }



    intArray(`x/in`) = if(intArray(`io_e`) > 0) { intArray(`io_a`) } else { intArray(`GEN_0`) }


    intArray(`clock/prev`) = intArray(`clock`)
    cycles += 1
  }
}

class GcdRunner {
  val tester = new GCDScalaImpl()

  val values =
    for {x <- 1 to 1000
         y <- 1 to 100
    } yield (x, y, computeGcd(x, y)._1)

  val startTime = System.nanoTime()
  tester.poke("clock", 1)

  for((x, y, z) <- values) {
    tester.step(1)
    tester.poke("io_a", x)
    tester.poke("io_b", y)
    tester.poke("io_e", 1)
    tester.step(1)

    tester.poke("io_e", 0)
    tester.step(1)

    while (tester.peek("io_v") != Big1) {
      tester.step(1)
    }

//    println(s"gcd($x, $y) => ${tester.peek("io_z")}, predicted => $z")
    assert(tester.peek("io_z") == z)

  }
  val endTime = System.nanoTime()
  val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

  println(s"${tester.cycles} cycles in elapsed seconds $elapsedSeconds")
  println(s"${tester.cycles.toDouble / (elapsedSeconds * 1000.0)} kHz")

}

object SimpleRunner {
  def main(args: Array[String]): Unit = {
    new GcdRunner
  }
}
