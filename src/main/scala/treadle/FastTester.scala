// See LICENSE for license details.

package treadle

import treadle.chronometry.Timer
import treadle.executable.{ScalaClassBuilder, SimplePokerPeeker}

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object FastTester {
  def apply(input: String, args: Array[String]): SimplePokerPeeker = {
    val optionsManager = new TreadleOptionsManager

    if(optionsManager.parse(args)) {
      val timer = new Timer
      val scalaTesterSource = timer("generate") {
        ScalaClassBuilder.makeScalaSource(input, optionsManager)
      }

      // println(scalaTesterSource)

      val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

      val evald = timer("compile") {
        val parsed = tb.parse(scalaTesterSource)
        tb.eval(parsed).asInstanceOf[SimplePokerPeeker]
      }

      println(timer.report)

      evald
    }
    else {
      throw new Exception(s"Could not parse args")
    }
  }
}
