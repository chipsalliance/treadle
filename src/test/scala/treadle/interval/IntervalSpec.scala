// See README.md for license details.

package treadle.interval

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.FreeSpec
import treadle.TreadleTester

//scalastyle:off regex
class IntervalSpec extends FreeSpec {
  "intervals should behave as follows" - {
    "clip behavior" in {
      val input =
        s"""circuit Unit :
           |  module Unit :
           |    input  s:     SInt<2>
           |    input  u:     UInt<3>
           |    input  in1:   Interval[-3, 5].0
           |    output wrap3: Interval
           |    output wrap5: Interval
           |    output wrap6: Interval
           |    output wrap7: Interval
           |    output clip3: Interval
           |    output clip4: Interval
           |    output clip5: Interval
           |    output clip6: Interval
           |    output clip7: Interval
           |    wrap3 <= wrap(in1, asInterval(s, -2, 4, 0))
           |    wrap5 <= wrap(in1, asInterval(s, -4, 4, 0))
           |    wrap6 <= wrap(in1, asInterval(s, -1, 7, 0))
           |    wrap7 <= wrap(in1, asInterval(s, -4, 7, 0))
           |    clip3 <= clip(in1, asInterval(s, -2, 4, 0))
           |    clip4 <= clip(in1, asInterval(s, -1, 1, 0))
           |    clip5 <= clip(in1, asInterval(s, -4, 4, 0))
           |    clip6 <= clip(in1, asInterval(s, -1, 7, 0))
           |    clip7 <= clip(in1, asInterval(s, -4, 7, 0))
           |    """.stripMargin

      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

      println("  in1      w(-2, 4, 0) w(-4, 4, 0) w(-1, 7, 0) w(-4, 7, 0) c(-2, 4, 0) c(-1, 1, 0) c(-4, 4, 0) c(-1, 7, 0) c(-4, 7, 0)")

      def show(input: Int): Unit = {
        val seq = Seq("wrap3", "wrap5", "wrap6", "wrap7", "clip3", "clip4", "clip5", "clip6", "clip7")
        println(f"$input%4d       " + seq.map { s => f"   ${tester.peek(s)}%3d      " }.mkString(""))
      }
      for(in1 <- -5 to 9) {
        tester.poke("in1", in1)
        show(in1)
      }
    }
  }

}
