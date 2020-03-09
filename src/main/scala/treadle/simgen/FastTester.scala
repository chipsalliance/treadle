// See LICENSE for license details.

package treadle.simgen

import java.io.PrintWriter

import treadle.TreadleOptionsManager
import treadle.chronometry.Timer

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

      optionsManager.makeTargetDir()

      val writer = new PrintWriter(optionsManager.commonOptions.targetDirName + "/generatedCode.scala")
      writer.println(scalaTesterSource)
      writer.close()

      val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

      val evald = timer("compile") {
        val parsed = tb.parse(scalaTesterSource)
        tb.eval(parsed).asInstanceOf[treadle.simgen.SimplePokerPeeker]
      }

      println(timer.report())

      evald
    }
    else {
      throw new Exception(s"Could not parse args")
    }
  }
}
