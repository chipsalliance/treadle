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

import firrtl.ir.Type
import treadle._
import treadle.executable._

/** This black-boxes a Clock Divider by 2.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  output  clk_out Divided Clock
  *  input   clk_in  Clock Input
  *
  */
class ClockDivider2(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "ClockDivider2"

  var clockOut: BigInt = Big0

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    clockOut
  }

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      clockOut = if (clockOut > Big0) Big0 else Big1
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
      "clk_out" -> Set("clk_in")
    )
  }
}

/** This black-boxes a Clock Divider by 3.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  *
  *  output  clk_out Divided Clock
  *  input   clk_in  Clock Input
  *
  */
class ClockDivider3(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "ClockDivider3"

  var clockOut: BigInt = Big1
  var delay:    BigInt = Big0

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    clockOut
  }

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      if (clockOut == Big0) {
        clockOut = Big1
        delay = Big0
      } else if (delay == Big1) {
        clockOut = Big0
        delay = Big0
      } else {
        delay = Big1
      }
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
      "clk_out" -> Set("clk_in")
    )
  }
}
