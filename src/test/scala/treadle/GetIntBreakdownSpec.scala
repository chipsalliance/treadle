// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


/**
  * The author of this fine test, does not quite remember what it is designed to do.
  * As it currently stands it is a decent demonstration of dead code removal. All but one
  * of the nodes generated at line 36 are dead-ends and do not contribute to the output.
  * To see this uncomment the println at line 44 and set showFirrtlAtLoad to true at line 48
  *
  * The other point of this test is to verify that the older form of writing a test using anonymous subclasses works
  */
// scalastyle:off magic.number
class GetIntBreakdownSpec extends FreeSpec with Matchers {
  def getIntBreakDown(n: Int): Boolean = {
    val input =
      """
        |circuit GetIntBreakdown :
        |  module GetIntBreakdown :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_in : UInt<30>
        |    output io_out : UInt<30>
        |
      """.stripMargin

    val s = new StringBuilder
    var lastNode = "io_in"
    s ++= "\n"
    for(i <- 0 until n) {
//      s ++= s"""    node T_$i = add($lastNode, UInt<1>("h1"))\n"""
//      s ++= s"""    node T_$i = add(io_in, UInt<1>("h1"))\n"""
      s ++= s"""    node T_$i = tail(add(io_in, UInt<1>("h1")), 1)\n"""
//      s ++= s"""    node T_$i = tail(io_in, 1)\n"""
      lastNode = s"T_$i"
    }
    s ++= s"    io_out <= $lastNode\n"

    val firrtlString = input ++ s.toString()

    // println(firrtlString.split("\n").zipWithIndex.map { case (l,n) => f"$n%5d $l"}.mkString("\n"))

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        showFirrtlAtLoad = false,
        setVerbose = false,
        rollbackBuffers = 0
      )
    }

    new TreadleTester(firrtlString, manager) {
      for (i <- 0 to 1000) {
        poke("io_in", 4)
        step(1000)
        peek("io_out")
        expect("io_out", 5)
      }
    }
    true
  }

  "GetIntBreakdownSpec should pass a basic test" in {
    getIntBreakDown(1000) should be(true)
  }
}
