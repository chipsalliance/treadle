// See LICENSE for license details.

package treadle

import org.scalatest.{Matchers, FlatSpec}

class ModuleInLineSpec extends FlatSpec with Matchers {
  behavior of "multiple modes"

  it should "expand instances as found" in {
    val stream = getClass.getResourceAsStream("/three_deep.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val tester = new TreadleTester(input)

    tester.engine.symbolTable.outputPortsNames.size should be > 0
  }
}
