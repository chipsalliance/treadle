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

import treadle.executable.TreadleException

import scala.util.matching.Regex

object PlusArg {
  val ReceiverLinePattern: Regex = """(\w*)=%\d?(.*)""".r
  val CommandLinePattern: Regex = """[+](\w*)=(.*)""".r

  def apply(s: String): PlusArg = {
    s match {
      case CommandLinePattern(key, argType) =>
        PlusArg(key, argType)
      case _ =>
        throw TreadleException(
          s"""Error: parsing plus-arg "$s" not of the form "+<name>=%<type>" """
        )
    }
  }
}

/** Convenience class that holds a parsed Plus Arg
  * Command line form of a plus_arg is +<name>=<value>
  */
case class PlusArg(name: String, value: String)
