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

import firrtl.MemKind
import logger.LazyLogging
import treadle.utils.NumberHelpers

import scala.util.matching.Regex

/** Controls whether a given memory cell should be logged to vcd output
  * if logAllRadixOpt is defined then all indices for all memories should be logged
  * otherwise if info has an entry then check the memory index
  * if the set contains -1 then all indices should be logged for that memory
  *
  * @param logAllRadixOpt  if defined then log all memories using this radix
  * @param memoriesTracked    map of memories to the set of indices that should be logged
  */
class VcdMemoryLoggingController(
  logAllRadixOpt:  Option[Int] = None,
  memoriesTracked: Map[Symbol, IndicesAndRadix] = Map.empty
) {

  private def indexedName(name: String, index: Int, radix: Int): String = {
    s"$name(${BigInt(index).toString(radix)})"
  }

  /* generate a vcd element name for a given memory location
     checking whether the memory and the particular offset is being tracked
   */
  def vcdKey(symbol: Symbol, offset: Int): Option[String] = {
    logAllRadixOpt match {
      case Some(radix) =>
        Some(indexedName(symbol.name, offset, radix))
      case _ =>
        memoriesTracked.get(symbol) match {
          case Some(IndicesAndRadix(set, radix)) =>
            if (set.contains(-1) || set.contains(offset)) {
              Some(indexedName(symbol.name, offset, radix))
            } else {
              None
            }
          case _ => None
        }
    }
  }

  /* Builds a list of all tracked memories and the locations within them that are tracked
   */
  def getIndexedNames(memorySymbol: Symbol): Seq[String] = {
    memoriesTracked.get(memorySymbol) match {
      case Some(IndicesAndRadix(set, radix)) =>
        set.map { offset =>
          indexedName(memorySymbol.name, offset, radix)
        }.toSeq
      case None => Seq.empty
    }
  }
}

object VcdMemoryLoggingController extends LazyLogging {
  private val radixChars = "bodhx"
  val GlobalRadix:        Regex = s"""all:?([$radixChars]?)""".r
  val SymbolAllRegex:     Regex = s"""([^:]+):all:?([$radixChars]?)""".r
  val SymbolIndicesRegex: Regex = """([^:]+):([^:]+)""".r

  private def getGlobalRadix(commands: Seq[String]): Option[Int] = {
    commands.collectFirst { case GlobalRadix(radix) => NumberHelpers.radixFromString(radix) }
  }

  /** Build a VcdMemoryLoggingController from a list of commands
    *
    * @param commands    a list of strings containing logging instructions
    * @param symbolTable use to check for symbols that are memories
    * @return
    */
  def apply(commands: Seq[String], symbolTable: SymbolTable): VcdMemoryLoggingController = {
    getGlobalRadix(commands) match {
      case Some(radix) =>
        // if here we are logging all indices for all memories
        new VcdMemoryLoggingController(Some(radix))
      case _ =>
        // if we are here we limiting to particular memories
        val memorySymbols = symbolTable.symbols.filter(_.dataKind == MemKind)

        def getEntry(
          memoryRegex:     String,
          indicesAndRadix: IndicesAndRadix
        ): Option[(Symbol, IndicesAndRadix)] = {

          val regex = memoryRegex.r
          memorySymbols.find(memorySymbol => regex.findFirstIn(memorySymbol.name).isDefined) match {
            case Some(memorySymbol) =>
              Some(memorySymbol -> indicesAndRadix)
            case None =>
              throw TreadleException(s"MemoryToVCD pattern $memoryRegex did not match any memories")
          }
        }

        new VcdMemoryLoggingController(
          logAllRadixOpt = None,
          commands.flatMap {
            case SymbolAllRegex(memoryRegex, radix) =>
              getEntry(memoryRegex, IndicesAndRadix(Set(-1), NumberHelpers.radixFromString(radix)))
            case SymbolIndicesRegex(memoryRegex, indicesString) =>
              val indicesAndRadix = NumberHelpers.parseIntRangeWithRadix(indicesString)
              getEntry(memoryRegex, indicesAndRadix)

          }.toMap
        )
    }
  }
}

case class IndicesAndRadix(indices: Set[Int], radix: Int = 10)
