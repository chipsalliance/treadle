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
import treadle.ScalaBlackBox
import treadle.executable.{PositiveEdge, Transition}

class SimDTM(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "SimDTM"

  var reset: BigInt = 0

  var debug_req_valid:     BigInt = 0 // output
  var debug_req_ready:     BigInt = 0 // input
  val debug_req_bits_addr: BigInt = 0 // output
  val debug_req_bits_op:   BigInt = 0 // output
  val debug_req_bits_data: BigInt = 0 // output

  var debug_resp_valid:     BigInt = 0 // input
  var debug_resp_ready:     BigInt = 0 // output
  var debug_resp_bits_resp: BigInt = 0 // input
  var debug_resp_bits_data: BigInt = 0 // input

  var exit: BigInt = 0 // output

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
      case "debug_req_valid"     => debug_req_valid
      case "debug_req_bits_addr" => debug_req_bits_addr
      case "debug_req_bits_op"   => debug_req_bits_op
      case "debug_req_bits_data" => debug_req_bits_data
      case "debug_resp_ready"    => debug_resp_ready
      case "exit"                => exit
      case _                     => BigInt(0)
    }
  }

  override def inputChanged(name: String, value: BigInt): Unit = {
    name match {
      case "reset"                => reset = value
      case "debug_req_ready"      => debug_req_ready = value
      case "debug_resp_valid"     => debug_resp_valid = value
      case "debug_resp_bits_resp" => debug_resp_bits_resp = value
      case "debug_resp_bits_data" => debug_resp_bits_data = value
      case _                      =>
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
        exit = 0
        debug_req_valid = 0
        debug_resp_ready = 0
      } else {
        exit = catBits(
          debug_req_valid,
          debug_req_ready,
          debug_req_bits_addr,
          debug_req_bits_op,
          debug_req_bits_data,
          debug_resp_valid,
          debug_resp_ready,
          debug_resp_bits_resp,
          debug_resp_bits_data
        )
      }
    }
  }

  /**
    * returns a list of names of inputs that this output depends on.
    *
    * @note The order of this list will determine the order of the inputValues argument to the getOutput method
    * @param outputName the output whose dependencies are being described
    * @return
    */
  //TODO: Figure out the correct dependencies here
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }
}
