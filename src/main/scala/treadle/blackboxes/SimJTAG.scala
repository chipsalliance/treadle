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

import firrtl.ir.{IntParam, Param, Type}
import treadle.ScalaBlackBox
import treadle.executable.{PositiveEdge, Transition, TreadleException}

/** Implements a scala black box corresponding to the verilog SimJtag.v used in rocket-chip
  * IO
  *
  * input         clock,
  * input         reset,
  *
  * input         enable,
  * input         init_done,
  *
  * output        jtag_TCK,
  * output        jtag_TMS,
  * output        jtag_TDI,
  * output        jtag_TRSTn,
  *
  * input         jtag_TDO_data,
  * input         jtag_TDO_driven,
  *
  * output [31:0] exit1
  *
  * @param instanceName identifies which instantiation this is
  */
class SimJTAG(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "SimJTAG"

  private def isSet(b: BigInt): Boolean = b != BigInt(0)

  // black box inputs
  var reset:            BigInt = 0
  var enable:           BigInt = 0
  var init_done:        BigInt = 0
  var init_done_sticky: BigInt = 0
  var jtag_TDO_data:    BigInt = 0
  var jtag_TDO_driven:  BigInt = 0

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "reset"           => reset = value
      case "enable"          => enable = value
      case "init_done"       => init_done = value
      case "jtag_TDO_data"   => jtag_TDO_data = value
      case "jtag_TDO_driven" => jtag_TDO_driven = value
      case _                 =>
    }
  }

  // black box outputs
  var jtag_TCK:          BigInt = 0
  val jtag_TMS:          BigInt = 0
  val jtag_TDI:          BigInt = 0
  val jtag_TRSTn:        BigInt = 0
  var exit:              BigInt = 0

  /**
    * @param inputValues Don't use these
    * @param tpe         The concrete type of this output
    * @param outputName  The name of this output
    * @return Computed current concrete value for the name output
    */
  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    outputName match {
      case "jtag_TCK"   => jtag_TCK
      case "jtag_TMS"   => jtag_TMS
      case "jtag_TDI"   => jtag_TDI
      case "jtag_TRSTn" => jtag_TRSTn
      case "exit"       => exit
      case _ =>
        throw TreadleException(s"SimJTAG($instanceName).getOutput($outputName). No such output defined")
    }
  }

  var tickCounterRegister: BigInt = 0
  var tickCounterNext:     BigInt = 0
  var tickDelay:           BigInt = 0

  //scalastyle:off method.name
  def jtag_TDO: BigInt = if (isSet(jtag_TDO_driven)) { jtag_TDO_data } else { util.Random.nextInt(2) }

  private def catBits(bits: BigInt*): BigInt = {
    bits.foldLeft(BigInt(0)) { (previousResult, bit) =>
      (previousResult << 1) | bit
    }
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      if (reset > BigInt(0)) {
        exit = 0
        tickCounterRegister = tickDelay
        init_done_sticky = 1
        jtag_TCK = if (jtag_TCK > BigInt(0)) { 1 } else { 0 }
      } else {
        init_done_sticky = init_done_sticky | init_done
        if (enable > BigInt(0) && init_done_sticky > BigInt(0)) {
          tickCounterRegister = tickCounterNext
          if (tickCounterRegister == BigInt(0)) {
            exit = catBits(
              jtag_TCK,
              jtag_TMS,
              jtag_TDI,
              jtag_TRSTn,
              jtag_TDO
            )
          }
        }
      }
    } else {
      tickCounterNext = if (tickCounterRegister < BigInt(1)) { tickDelay } else { tickCounterRegister - 1 }
    }
  }

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case IntParam("TICK_DELAY", value) => tickDelay = value
      case _                             =>
    }
  }

  /**
    * returns a list of names of inputs that this output depends on.
    *
    * @note The order of this list will determine the order of the inputValues argument to the getOutput method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq("reset", "enable", "init_done", "init_done_sticky", "jtag_TDO_data", "jtag_TDO_driven")
  }
}
