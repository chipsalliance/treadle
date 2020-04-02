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

package treadle.blackboxes

import treadle.{ScalaBlackBox, ScalaBlackBoxFactory}

class BuiltInBlackBoxFactory extends ScalaBlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "AsyncResetReg" =>
        Some(add(new AsyncResetReg(instanceName)))
      case "ClockDivider2" =>
        Some(add(new ClockDivider2(instanceName)))
      case "ClockDivider3" =>
        Some(add(new ClockDivider3(instanceName)))
      case "EICG_wrapper" =>
        Some(add(new EicgWrapper(instanceName)))
      case "plusarg_reader" =>
        Some(add(new PlusArgReader(instanceName)))
      case "RoccBlackBox" =>
        Some(add(new RoccBlackBox(instanceName)))
      case "SimDTM" =>
        Some(add(new SimDTM(instanceName)))
      case "SimJTAG" =>
        Some(add(new SimJTAG(instanceName)))
      case _ =>
        None
    }
  }
}
