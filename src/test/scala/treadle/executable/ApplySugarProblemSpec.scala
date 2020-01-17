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

package treadle.executable

import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class ApplySugarProblemSpec extends FreeSpec with Matchers {
  type IntFunc = () => Int

  trait HasIntFunc {
    def apply: FuncInt
  }

  class Add(f1: IntFunc, f2: IntFunc, verbose: Boolean) extends HasIntFunc {
    val apply: FuncInt = if (verbose) { () =>
      {
        println("silent")
        f1() + f2()
      }
    } else { () =>
      {
        val (r1, r2) = (f1(), f2())
        println(s"add($r1, $r2) => ${r1 + r2}")
        r1 + r2
      }
    }
  }

  "should work" in {
    val add = new Add(() => 3, () => 5, false)
    println(s"start")
    add.apply() should be(8)
    println(s"after apply")
    val addv = new Add(() => 3, () => 5, true)
    addv.apply() should be(8)
    println(s"after apply")
  }
}
