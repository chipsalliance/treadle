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

package treadle.executable

import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir.ClockType

/**
  * builds driving and driven by relationships between symbols
  */
class SensitivityGraphBuilder {
  val childrenOf: MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]
  val parentsOf:  MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]

  def addSensitivity(drivingSymbol: Symbol, sensitiveSymbol: Symbol): Unit = {
    if (!childrenOf.contains(drivingSymbol)) childrenOf.addVertex(drivingSymbol)
    if (!childrenOf.contains(sensitiveSymbol)) childrenOf.addVertex(sensitiveSymbol)
    if (!parentsOf.contains(drivingSymbol)) parentsOf.addVertex(drivingSymbol)
    if (!parentsOf.contains(sensitiveSymbol)) parentsOf.addVertex(sensitiveSymbol)

    childrenOf.addPairWithEdge(drivingSymbol, sensitiveSymbol)
    parentsOf.addPairWithEdge(sensitiveSymbol, drivingSymbol)
  }

  def getChildrenOfDiGraph: DiGraph[Symbol] = DiGraph(childrenOf)
  def getParentsOfDiGraph:  DiGraph[Symbol] = DiGraph(parentsOf)

  /**
    * Find all sources that are not inputs or registers.  These should be initialized
    * once at beginning of simulation
    * @param symbolTable used for testing properties of symbols
    * @return
    */
  def orphans(symbolTable: SymbolTable): Seq[Symbol] = {
    val o2 = childrenOf.findSources.filterNot { symbol =>
      symbolTable.isTopLevelInput(symbol.name) ||
      symbolTable.isRegister(symbol.name) ||
      symbol.firrtlType == ClockType
    }.toSeq
    o2
  }
}
