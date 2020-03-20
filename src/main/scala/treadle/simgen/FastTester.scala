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

      val testRunner = timer("compile") {
        val parsed = tb.parse(scalaTesterSource)
        tb.eval(parsed).asInstanceOf[treadle.simgen.SimplePokerPeeker]
      }

      println(timer.report())

      testRunner
    }
    else {
      throw new Exception(s"Could not parse args")
    }
  }
}
