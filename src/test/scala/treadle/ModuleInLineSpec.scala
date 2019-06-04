// See LICENSE for license details.

package treadle

import org.scalatest.{Matchers, FlatSpec}

class ModuleInLineSpec extends FlatSpec with Matchers {
  behavior of "multiple modes"

  it should "expand instances as found" in {
    val stream = getClass.getResourceAsStream("/three_deep.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val tester = TreadleTester(input)

    tester.engine.symbolTable.outputPortsNames.size should be > 0
  }

  it should "nester registers should all be using the same clock" in {
    val stream = getClass.getResourceAsStream("/NestedModsWithReg.fir")
    val input = io.Source.fromInputStream(stream).mkString

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        callResetAtStartUp = false
      )
    }
    val tester = TreadleTester(input, optionsManager)

    tester.poke("in1", 3)
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")
    tester.step()
    println(s"out1 ${tester.peek("out1")} out3 ${tester.peek("out2")} out3 ${tester.peek("out3")}")

    tester.report()
  }
}
