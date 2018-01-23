//// See LICENSE for license details.
//
//treadle treadle.real
//
//import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}
//import treadle._
//import org.scalatest.{FreeSpec, Matchers}
//
//
//class BlackBoxRealSpec extends FreeSpec with Matchers {
//  "this tests black box implmentation of real numbers" - {
//    val adderInput =
//      """
//        |circuit RealAdder :
//        |  extmodule BBFAdd :
//        |    output out : UInt<64>
//        |    input in2 : UInt<64>
//        |    input in1 : UInt<64>
//        |
//        |
//        |  module RealAdder :
//        |    input clk : Clock
//        |    input reset : UInt<1>
//        |    input io_a1_node : UInt<64>
//        |    input io_a2_node : UInt<64>
//        |    output io_c_node : UInt<64>
//        |
//        |    reg register1_node : UInt<64>, clk with :
//        |      reset => (UInt<1>("h0"), register1_node)
//        |    inst BBFAdd_1 of BBFAdd @[DspReal.scala 82:36]
//        |    wire T_15_node : UInt<64> @[DspReal.scala 67:19]
//        |    io_c_node <= register1_node
//        |    register1_node <= T_15_node
//        |    BBFAdd_1.in2 <= io_a2_node
//        |    BBFAdd_1.in1 <= io_a1_node
//        |    T_15_node <= BBFAdd_1.out
//      """.stripMargin
//
//    "addition should work expand instances as found" in {
//
//      val optionsManager = new InterpreterOptionsManager {
//        interpreterOptions = treadleOptions.copy(blackBoxFactories = Seq(new DspRealFactory), randomSeed = 0L)
//      }
//      val tester = new InterpretiveTester(adderInput, optionsManager)
//      tester.interpreter.verbose = true
//      tester.interpreter.setVerbose()
//
//
//      tester.poke("io_a1_node", doubleToBigIntBits(1.5))
//      tester.poke("io_a2_node", doubleToBigIntBits(3.25))
//      tester.step()
//
//      tester.expect("io_c_node", doubleToBigIntBits(4.75))
//    }
//
//    "poison should propagate through black boxes" in {
//      val optionsManager = new InterpreterOptionsManager {
//        interpreterOptions = treadleOptions.copy(blackBoxFactories = Seq(new DspRealFactory), randomSeed = 0L)
//      }
//      val tester = new InterpretiveTester(adderInput, optionsManager)
//      tester.interpreter.verbose = true
//      tester.interpreter.setVerbose()
//
//      tester.peekConcrete("io_a1_node").poisoned should be (true)
//      tester.peekConcrete("io_a2_node").poisoned should be (true)
//      tester.peekConcrete("io_c_node").poisoned should be (true)
//      tester.step()
//
//      tester.peekConcrete("io_a1_node").poisoned should be (true)
//      tester.peekConcrete("io_a2_node").poisoned should be (true)
//      tester.peekConcrete("io_c_node").poisoned should be (true)
//    }
//  }
//
//  "truncation is supported with IntPart" in {
//    val input =
//      """
//        |circuit Trunc :
//        |  extmodule BBFIntPart :
//        |    output out : UInt<64>
//        |    input in : UInt<64>
//        |
//        |  module Trunc :
//        |    input clk : Clock
//        |    input reset : UInt<1>
//        |    input io_a_node : UInt<64>
//        |    output io_c_node : UInt<64>
//        |
//        |    inst BBFIntPart_1 of BBFIntPart @[DspReal.scala 82:36]
//        |    io_c_node <= BBFIntPart_1.out
//        |    BBFIntPart_1.in <= io_a_node
//      """.stripMargin
//
//    val optionsManager = new InterpreterOptionsManager {
//      interpreterOptions = treadleOptions.copy(blackBoxFactories = Seq(new DspRealFactory))
//    }
//    val tester = new InterpretiveTester(input, optionsManager)
//    tester.interpreter.verbose = true
//    tester.interpreter.setVerbose()
//
//
//    tester.poke("io_a_node", doubleToBigIntBits(3.14159))
//
//    tester.expect("io_c_node", doubleToBigIntBits(3.0))
//  }
//}