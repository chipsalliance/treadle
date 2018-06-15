// See LICENSE for license details.

package treadle.chronometry

import firrtl.{Driver, ExecutionOptionsManager}
import firrtl.{FirrtlExecutionSuccess, HasFirrtlOptions, Parser}
import firrtl.passes.CheckChirrtl
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.ClockInfo
import treadle.{TreadleOptionsManager, TreadleTester}
import treadle.utils.ToLoFirrtl

//noinspection ScalaStyle
class ClockCrossingSpec extends FreeSpec with Matchers {

  "clocks generated using asClock should function properly by advancing their associated registers" in {
    val chirrtlString =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.11.12, sbtVersion: 1.1.1
        |circuit ClockCrossing :
        |  module ClockCrossing :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip divIn : UInt<8>, mainOut : UInt<8>}
        |
        |    reg divClock : UInt<1>, clock with : (reset => (reset, UInt<1>("h01"))) @[ClockCrossingTest.scala 22:29]
        |
        |    node _T_12 = eq(divClock, UInt<1>("h00")) @[ClockCrossingTest.scala 23:19]
        |    divClock <= _T_12 @[ClockCrossingTest.scala 23:16]
        |
        |    wire divRegWire : UInt @[ClockCrossingTest.scala 25:28]
        |    node _T_14 = asClock(divClock) @[ClockCrossingTest.scala 26:26]
        |
        |    reg _T_17 : UInt, _T_14 with : (reset => (reset, UInt<1>("h01"))) @[ClockCrossingTest.scala 27:29]
        |
        |    _T_17 <= io.divIn @[ClockCrossingTest.scala 27:29]
        |    divRegWire <= _T_17 @[ClockCrossingTest.scala 28:20]
        |
        |    reg mainReg : UInt, clock with : (reset => (reset, UInt<1>("h00"))) @[ClockCrossingTest.scala 31:28]
        |    mainReg <= divRegWire @[ClockCrossingTest.scala 31:28]
        |
        |    io.mainOut <= mainReg @[ClockCrossingTest.scala 32:18]
        |
      """.stripMargin

    val firrtlOptionsManager = new ExecutionOptionsManager("test") with HasFirrtlOptions {
      firrtlOptions = firrtlOptions.copy(firrtlSource = Some(chirrtlString), compilerName = "low")
    }

    Driver.execute(firrtlOptionsManager) match {
      case FirrtlExecutionSuccess(_, highFirrtl) =>
        val optionsManager = new TreadleOptionsManager {
          commonOptions = commonOptions.copy(targetDirName = "test_run_dir/clock_crossing")
          treadleOptions = treadleOptions.copy(
            setVerbose = true, writeVCD = true, callResetAtStartUp = false,
            showFirrtlAtLoad = false,
            clockInfo = Seq(ClockInfo(name = "clock", period = 2, initialOffset = 1)))
        }

        val tester = new TreadleTester(highFirrtl, optionsManager)

        tester.reset(8)

        tester.poke("io_divIn", 0x42)
        tester.expect("io_mainOut", 0)  // initial register value
        tester.step()
        tester.expect("io_mainOut", 1)  // initial value of divReg
        tester.step()  // for divided clock to have a rising edge
        tester.expect("io_mainOut", 1)  // one-cycle-delay divReg
        tester.step()  // for main clock register to propagate
        tester.expect("io_mainOut", 0x42)  // updated value propagates
        tester.finish
      case _ =>

    }
  }
}
