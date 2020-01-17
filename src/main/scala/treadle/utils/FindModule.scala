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

package treadle.utils

import firrtl.ir.{Circuit, DefModule, Module}
import treadle.executable.TreadleException

object FindModule {

  /**
    * finds the specified module name in the circuit
    *
    * @param moduleName name to find
    * @param circuit circuit being analyzed
    * @return the circuit, exception occurs in not found
    */
  def apply(moduleName: String, circuit: Circuit): DefModule = {
    circuit.modules.find(module => module.name == moduleName) match {
      case Some(module: Module) =>
        module
      case Some(externalModule: DefModule) =>
        externalModule
      case _ =>
        throw TreadleException(s"Could not find module $moduleName in circuit $circuit")
    }
  }
}
