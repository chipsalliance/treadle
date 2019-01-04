// See LICENSE for license details.

package treadle

import treadle.OnTheFly.getClass
import treadle.executable.{ScalaClassBuilder, SimplePokerPeeker}

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object FastTester {
  def apply(input: String, args: Array[String]): SimplePokerPeeker = {
    val optionsManager = new TreadleOptionsManager

    if(optionsManager.parse(args)) {
      val scalaTesterSource = ScalaClassBuilder.makeScalaSource(input, optionsManager)

      println(scalaTesterSource)

      val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

      val parsed = tb.parse(scalaTesterSource)
      val evald = tb.eval(parsed).asInstanceOf[SimplePokerPeeker]
      evald
    }
    else {
      throw new Exception(s"Could not parse args")
    }
  }
}
