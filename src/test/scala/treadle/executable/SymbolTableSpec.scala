// See LICENSE for license details.

package treadle.executable

import firrtl.CommonOptions
import firrtl.graph.CyclicException
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.transforms.DontCheckCombLoopsAnnotation
import treadle._
import org.scalatest.{FreeSpec, Matchers}
import treadle.chronometry.UTC

//scalastyle:off magic.number
class SymbolTableSpec extends FreeSpec with Matchers {
  """SymbolTable creates a table with dependency information""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    input io_in2 : UInt<16>
         |    output io_out1 : UInt<16>
         |    output io_out2 : UInt<16>
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |    io_out1 <= a3
         |
         |    node b1 = io_in2
         |    node b2 = b1
         |    reg b3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), b3)
         |    b3 <= b2
         |    node b4 = b3
         |    io_out2 <= b4
    """
        .stripMargin

    val wallTime = new UTC()
    val simulator = TreadleTester(Seq(FirrtlSourceAnnotation(simpleFirrtl)))

    val symbolTable = simulator.engine.symbolTable

    val keyToDependent = symbolTable.childrenOf

    // b3, b3/in, and io_out2 depend on clock
    keyToDependent.reachableFrom(symbolTable("clock")).size should be (3)

    symbolTable.registerNames.toList.sorted.foreach { key =>
      val dependents = symbolTable.childrenOf.reachableFrom(symbolTable(key))

      println(s"$key => ${dependents.map(_.name).mkString(",")}")
    }

    println("All dependencies")
    symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
      val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

      println(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
    }
  }

  """SymbolTable understands direct combinational dependencies""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    output io_out1 : UInt<16>
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |    io_out1 <= a3
         """
        .stripMargin

    val optionsManager = new TreadleOptionsManager
    val tester = TreadleTester(simpleFirrtl, optionsManager)
    val simulator = tester.engine

    val symbolTable = simulator.symbolTable

    val childrenOf = symbolTable.childrenOf

    childrenOf.reachableFrom(symbolTable("clock")).size should be (0)

    childrenOf.reachableFrom(symbolTable("io_in1")) should contain (symbolTable("io_out1"))

    println("All dependencies")
    symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
      val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

      println(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
    }

    tester.poke("io_in1", 7)
    tester.expect("io_out1", 7)
    tester.poke("io_in1", 42)
    tester.expect("io_out1", 42)
  }

  """registers break dependency chain""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    output io_out1 : UInt<16>
         |
         |    reg a3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), a3)
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    a3 <= a2
         |    node a4 = a3
         |    io_out1 <= a4
         """
        .stripMargin

    val optionsManager = new TreadleOptionsManager
    val tester = TreadleTester(simpleFirrtl, optionsManager)
    val simulator = tester.engine

    val symbolTable = simulator.symbolTable

    val childrenOf = symbolTable.childrenOf

    // a3, a3/in and io_out1 depend on clock
    childrenOf.reachableFrom(symbolTable("clock")).size should be (3)

    childrenOf.reachableFrom(symbolTable("io_in1")) should not contain symbolTable("io_out1")

    println("All dependencies")
    symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
      val dependents = symbolTable.childrenOf.reachableFrom(keySymbol)

      println(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
    }

    tester.poke("io_in1", 7)
    tester.step()
    tester.expect("io_out1", 7)
    tester.poke("io_in1", 42)
    tester.step()
    tester.expect("io_out1", 42)
  }

  """This is a mixed chain""" in {
    val simpleFirrtl: String =
      s"""
         |circuit Simple :
         |  module Simple :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_in1 : UInt<16>
         |    input io_in2 : UInt<16>
         |    output io_out1 : UInt<17>
         |
         |    reg b3 : UInt<16>, clock with :
         |      reset => (UInt<1>("h0"), b3)
         |
         |    node a1 = io_in1
         |    node a2 = a1
         |    node a3 = a2
         |
         |    node b1 = io_in2
         |    node b2 = b1
         |    b3 <= b2
         |
         |    node a4 = add(a3, b3)
         |    io_out1 <= a4
         """
        .stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(setVerbose = false)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }
    val tester = TreadleTester(simpleFirrtl, optionsManager)
    val simulator = tester.engine

    val symbolTable = simulator.symbolTable

    val childrenOf = symbolTable.childrenOf

    childrenOf.reachableFrom(symbolTable("clock")).size should be (3)

    childrenOf.reachableFrom(symbolTable("io_in1")) should contain (symbolTable("io_out1"))
    childrenOf.reachableFrom(symbolTable("io_in2")) should not contain symbolTable("io_out1")
    childrenOf.reachableFrom(symbolTable("io_in2")) should contain (symbolTable("b3/in"))
    childrenOf.reachableFrom(symbolTable("b3")) should contain (symbolTable("io_out1"))

    val inputChildren = symbolTable
      .getChildren(symbolTable.inputPortsNames.map(symbolTable(_)).toSeq)
      .toList
      .sortBy(_.cardinalNumber)

    println("Input dependencies")
    println(inputChildren.map(s => s"${s.name}:${s.cardinalNumber}").mkString(","))

    tester.poke("io_in1", 0)
    tester.poke("io_in2", 0)
    tester.step()
    tester.poke("io_in1", 7)
    tester.poke("io_in2", 4)
    tester.expect("io_out1", 7)
    tester.step()
    tester.expect("io_out1", 11)
    tester.poke("io_in1", 42)
    tester.expect("io_out1", 46)
    tester.poke("io_in2", 33)
    tester.expect("io_out1", 46)
    tester.step()
    tester.expect("io_out1", 75)
    tester.report()
  }

  """Should report combinational loop""" in {
    val simpleFirrtl: String =
      s"""
          |circuit HasLoop :
          |  module SubModule :
          |    input in1 : UInt<1>
          |    input in2 : UInt<16>
          |    input in3 : UInt<16>
          |    output out1 : UInt<16>
          |
          |    out1 <= mux(in1, in2, in3)
          |
          |  module HasLoop :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_in1 : UInt<16>
          |    input io_in2 : UInt<16>
          |    output io_out1 : UInt<17>
          |
          |    reg b3 : UInt<16>, clock with :
          |      reset => (UInt<1>("h0"), b3)
          |
          |    node a1 = io_in1
          |    node a2 = io_in2
          |    b3 <= a2
          |
          |    inst sub of SubModule
          |
          |    sub.in1 <= io_in1
          |    sub.in2 <= io_out1
          |    sub.in3 <= b3
          |
          |    io_out1 <= sub.out1
       """
              .stripMargin

    val optionsManager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(setVerbose = false)
      firrtlOptions = firrtlOptions.copy(annotations = firrtlOptions.annotations :+ DontCheckCombLoopsAnnotation)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }

    try {
      TreadleTester(simpleFirrtl, optionsManager)
    }
    catch {
      case c: CyclicException =>
        c.node.asInstanceOf[Symbol].name should be ("sub.out1")

    }
  }
}
