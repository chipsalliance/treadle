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

package treadle.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.Phase
import firrtl.stage.FirrtlCircuitAnnotation
import treadle.{TreadleCircuitStateAnnotation, TreadleTester, TreadleTesterAnnotation}

object CreateTester extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    if (a.exists {
          case FirrtlCircuitAnnotation(_)       => true
          case TreadleCircuitStateAnnotation(_) => true
          case _                                => false
        }) {
      val tester = new TreadleTester(a)
      a :+ TreadleTesterAnnotation(tester)
    } else {
      a
    }
  }
}
