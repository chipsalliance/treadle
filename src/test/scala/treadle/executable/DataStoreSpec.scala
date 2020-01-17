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

import treadle._
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.{BigIntTestValuesGenerator, DataStorePlugInAnnotation, TreadleTester}

import scala.collection.mutable

class DataStoreSpec extends FreeSpec with Matchers {

  info("this")

  "DataStore Plugins can be added via an annotation" - {
    "They can be useful for analytics on a circuit simulation" in {
      val input =
        """
          |circuit PassThrough :
          |  module PassThrough :
          |    input clock : Clock
          |    input a : SInt<8>
          |    input b : SInt<8>
          |    output c: SInt<9>
          |    output d: SInt<10>
          |
          |    reg r : SInt<9>, clock
          |    r <= add(a, b)
          |    c <= add(a, b)
          |    d <= add(r, a)
          |""".stripMargin

      case class Extrema(low: BigInt, high: BigInt) {
        def update(value: BigInt): Extrema = {
          if (value < low) { Extrema(value, high) } else if (value > high) { Extrema(low, value) } else { this }
        }
      }

      class DataCollector {
        val extrema = new mutable.HashMap[String, Extrema]

        def getPlugin(executionEngine: ExecutionEngine): DataStorePlugin = {
          PlugIn(executionEngine)
        }

        case class PlugIn(executionEngine: ExecutionEngine) extends DataStorePlugin {
          override def dataStore: DataStore = executionEngine.dataStore

          override def run(symbol: Symbol, offset: Int, previousValue: Big): Unit = {
            extrema(symbol.name) = extrema.get(symbol.name) match {
              case Some(extrema) => extrema.update(dataStore(symbol))
              case None          => Extrema(dataStore(symbol), dataStore(symbol))
            }
          }
        }
      }

      val dataCollector = new DataCollector
      val annos = Seq(
        DataStorePlugInAnnotation("DataCollector", dataCollector.getPlugin)
      )
      val tester = TreadleTester(annos :+ FirrtlSourceAnnotation(input))

      val extremes = extremaOfSIntOfWidth(8)
      for {
        a <- BigIntTestValuesGenerator(extremes)
        b <- BigIntTestValuesGenerator(extremes)
      } {
        tester.poke("a", a)
        tester.poke("b", b)
        tester.step()
      }
      tester.finish

      dataCollector.extrema("c") should be(Extrema(-256, 254))
      dataCollector.extrema("d") should be(Extrema(-384, 381))
      dataCollector.extrema("r") should be(Extrema(-256, 254))
    }
  }
}
