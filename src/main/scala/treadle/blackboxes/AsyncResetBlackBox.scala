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

import firrtl.ir.{Param, Type}
import treadle._
import treadle.executable._

/**
  * Implements a single bit register with asynchronous reset
  */
class AsyncResetReg(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "AsyncResetReg"

  var nextValue:    BigInt = Big0
  var currentValue: BigInt = Big0
  var resetValue:   BigInt = Big0
  var enable:       Boolean = false

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    currentValue
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "io_d"  => nextValue = value
      case "io_en" => enable = value > Big0
      case "rst" =>
        if (value > Big0) {
          nextValue = resetValue
          currentValue = nextValue
        }
      case _ =>
    }
    println(s"next $nextValue cur $currentValue, en $enable")
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge && enable) {
      currentValue = nextValue
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  /**
    * Important note: The dependency of io_d on io_q makes this test work by making sure
    * that the assignments are sorted correctly topologically.
    * They mirror a similar pattern used on registers, necessary for treadle to be able
    * to update the circuit in a single pass.
    * @return
    */
  override def getDependencies: Seq[(String, Set[String])] = {
    Seq(
      "io_d" -> Set("io_q"),
      "io_q" -> Set("rst", "clk")
    )
  }

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case firrtl.ir.IntParam("RESET_VALUE", value) =>
        resetValue = value
      case _ =>
      // ignore
    }
  }
}

class AsyncResetBlackBoxFactory extends ScalaBlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
    blackBoxName match {
      case "AsyncResetReg" =>
        Some(add(new AsyncResetReg(instanceName)))
      case _ =>
        None
    }
  }
}
