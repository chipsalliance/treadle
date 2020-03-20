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

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

trait CompileTreadleRunner {
  def run(): Unit
}

/** Takes a String and compiles it into a runnable class.
  */
object ScalaStringCompiler {
  def apply(scalaSource: String): CompileTreadleRunner = {

    try {
      val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

      val parsed = tb.parse(scalaSource)
      val evaluated = tb.eval(parsed)
      val runner = evaluated.asInstanceOf[treadle.simgen.CompileTreadleRunner]
      runner
    } catch {
      case t: Throwable => throw t
    }
  }
}
