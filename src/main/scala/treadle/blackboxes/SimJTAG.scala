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
import treadle.executable.{PositiveEdge, Transition}

class SimJTAG(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "SimJTAG"

  private def isSet(b: BigInt): Boolean = b != BigInt(0)

  var reset:            BigInt = 0
  var enable:           BigInt = 0
  var init_done:        BigInt = 0
  var init_done_sticky: BigInt = 0
  var jtag_TDO_data:    BigInt = 0
  var jtag_TDO_driven:  BigInt = 0

  var tickCounterRegister: BigInt = 0
  var tickCounterNext:     BigInt = 0
  var tickDelay:           BigInt = 0
  var __jtag_TCK:          BigInt = 0
  val __jtag_TMS:          BigInt = 0
  val __jtag_TDI:          BigInt = 0
  val __jtag_TRSTn:        BigInt = 0
  //scalastyle:off method.name
  def __jtag_TDO: BigInt = if (isSet(jtag_TDO_driven)) { jtag_TDO_data } else { util.Random.nextInt(2) }
  var __exit:     BigInt = 0

  /**
    * getOutput is called to determine the value for the named output at the
    * current state of the system. The proper way to do this is to not use the inputValues.
    * Instead use[[inputChanged]] to supply a black box with its inputs.
    *
    * @param inputValues This is a list of BigInt values that are in the same order
    *                    as the outputDependencies lists them
    * @param tpe         The concrete type of this output
    * @param outputName  The name of this output
    * @return Computed current concrete value for the name output
    */
  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    outputName match {
      case "jtag_TCK"   => __jtag_TCK
      case "jtag_TMS"   => __jtag_TMS
      case "jtag_TDI"   => __jtag_TDI
      case "jtag_TRSTn" => __jtag_TRSTn
      case "exit"       => __exit
      case _ =>
        BigInt(0)
    }
  }

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

  private def catBits(bits: BigInt*): BigInt = {
    bits.foldLeft(BigInt(0)) { (previousResult, bit) =>
      (previousResult << 1) | bit
    }
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {
    if (transition == PositiveEdge) {
      if (reset > BigInt(0)) {
        __exit = 0
        tickCounterRegister = tickDelay
        init_done_sticky = 1
        __jtag_TCK = if (__jtag_TCK > BigInt(0)) { 1 } else { 0 }
      } else {
        init_done_sticky = init_done_sticky | init_done
        if (enable > BigInt(0) && init_done_sticky > BigInt(0)) {
          tickCounterRegister = tickCounterNext
          if (tickCounterRegister == BigInt(0)) {
            __exit = catBits(
              __jtag_TCK,
              __jtag_TMS,
              __jtag_TDI,
              __jtag_TRSTn,
              __jtag_TDO
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
