// See LICENSE for license details.

package treadle

import firrtl.ir.{Circuit, DefModule, Module}

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
        throw InterpreterException(s"Could not find module $moduleName in circuit $circuit")
    }
  }
}
