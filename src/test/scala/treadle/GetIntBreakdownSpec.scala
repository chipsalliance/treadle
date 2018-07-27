// See LICENSE for license details.

package treadle

import org.scalatest.{FreeSpec, Matchers}


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

//    println(firrtlString.split("\n").zipWithIndex.map { case (l,n) => f"$n%5d $l"}.mkString("\n"))

    val tester = TreadleFactory(firrtlString, "--tr-rollback-buffers", "0")

    for(i <- 0 to 1000) {
      tester.poke("io_in", 4)
      tester.step(1000)
      tester.peek("io_out")
//      tester.expect("io_out", 4)
    }
    true
  }
  "GetIntBreakdownSpec should pass a basic test" ignore {
    getIntBreakDown(1000) should be(true)
  }
}
