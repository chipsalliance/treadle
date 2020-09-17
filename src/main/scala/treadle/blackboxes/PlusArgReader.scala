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

import firrtl.ir.{IntParam, Param, StringParam, Type}
import treadle.executable.{Transition, TreadleException}
import treadle.{ScalaBlackBox, _}

/** Allows overriding values at simulation time
  *
  * @param instanceName name assigned to instance
  */
class PlusArgReader(val instanceName: String) extends ScalaBlackBox {
  override def name: String = "plusarg_reader"

  var myPlus:      BigInt = Big0
  var mask:        BigInt = Big1
  var plusArgName: String = ""
  var plusArgType: String = ""

  override def inputChanged(name: String, value: BigInt): Unit = {}

  override def setParams(params: Seq[Param]): Unit = {
    params.foreach {
      case IntParam("DEFAULT", value) => myPlus = value
      case IntParam("WIDTH", value)   => mask = BigInt("1" * value.toInt, 2)
      case StringParam("FORMAT", pattern) =>
        pattern.string match {
          case PlusArg.ReceiverLinePattern(name, typ) =>
            plusArgName = name
            plusArgType = typ
          case _ =>
            throw TreadleException(
              s"""PlusArgReader("$instanceName) FORMAT="${pattern.string} not of the form "<name>=%<type>" """
            )
        }
      case _ =>
    }
  }

  override def setPlusArgs(plusArgs: Seq[PlusArg]): Unit = {
    if (plusArgName.nonEmpty) {
      plusArgs.foreach { arg =>
        if(arg.name == plusArgName) {
          try {
            myPlus = plusArgType match {
              case "b" => BigInt(arg.value, 2)
              case "o" => BigInt(arg.value, 8)
              case "d" => BigInt(arg.value, 10)
              case "h" => BigInt(arg.value, 16)
              case "x" => BigInt(arg.value, 16)
            }
          }
          catch {
            case t: Throwable =>
              val exception = TreadleException(
                s"""PlusArgReader("$instanceName) "$plusArgName=$plusArgType could not parse
                   |plusArg $arg""".stripMargin
              )
              exception.initCause(t)
              exception
          }
        }
      }
    }
  }

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    myPlus
  }

  override def clockChange(transition: Transition, clockName: String): Unit = {}

  // Don't use this.
  override def outputDependencies(outputName: String): Seq[String] = {
    Seq.empty
  }

  override def getDependencies: Seq[(String, collection.Set[String])] = {
    Seq.empty
  }
}
