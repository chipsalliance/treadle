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

import firrtl.ir.Info

case class StopOp(
  symbol:          Symbol,
  info:            Info,
  returnValue:     Int,
  condition:       IntExpressionResult,
  hasStopped:      Symbol,
  dataStore:       DataStore,
  clockTransition: ClockTransitionGetter
) extends Assigner {

  def run: FuncUnit = {
    val conditionValue = condition.apply() > 0
    if (conditionValue && clockTransition.isPosEdge) {
      if (dataStore(hasStopped) > 0) {
        if (isVerbose) {
          println(s"previous stop has fired with result ${dataStore(hasStopped)}")
        }
      } else {
        if (isVerbose) {
          println(s"stop ${symbol.name} has fired")
        }
        dataStore(hasStopped) = returnValue + 1
      }
    }

    () => ()
  }
}

object StopOp {
  val stopHappenedName = "/stopped"
}

case class StopInfo(stopSymbol: Symbol)
