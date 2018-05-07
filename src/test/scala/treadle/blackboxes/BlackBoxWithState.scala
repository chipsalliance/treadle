// See LICENSE for license details.

package treadle.blackboxes

import firrtl.ir.Type
import treadle.{BlackBoxFactory, BlackBoxImplementation, TreadleOptionsManager, TreadleTester}
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.{ClockInfo, PositiveEdge, Transition}


// scalastyle:off magic.number
/**
  * This test is pretty sensitive to when the reset comes down.
  * Hence the negative initialOffset
  */
class BlackBoxWithState extends FreeSpec with Matchers {
  "BlackBoxWithState should pass a basic test" in {
    val input =
      """
        |circuit AccumBlackBoxWrapper : @[:@2.0]
        |  extmodule AccumBlackBox : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@4.4]
        |    output data : UInt<16> @[:@5.4]
        |
        |    defname = AccumBlackBox
        |
        |
        |  module AccumBlackBoxWrapper : @[:@10.2]
        |    input clock : Clock @[:@11.4]
        |    input reset : UInt<1> @[:@12.4]
        |    output io_data : UInt<16> @[:@13.4]
        |
        |    inst m of AccumBlackBox @[AccumBlackBoxSpec.scala 93:17:@15.4]
        |    node _T_4 = bits(reset, 0, 0) @[AccumBlackBoxSpec.scala 96:9:@20.4]
        |    node _T_6 = eq(_T_4, UInt<1>("h0")) @[AccumBlackBoxSpec.scala 96:9:@21.4]
        |    io_data <= m.data
        |    m.clock <= clock
        |    m.reset <= reset
        |
      """.stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        setVerbose = false,
        writeVCD = false,
        symbolsToWatch = Seq("io_data"),
        blackBoxFactories = Seq(new AccumBlackBoxFactory),
        callResetAtStartUp = true,
        clockInfo = Seq(ClockInfo("clock", 10, -2))
      )
    }
    val tester = new TreadleTester(input, manager)

    val initialValue = tester.peek("io_data")
    println(s"Initial value is $initialValue")
    tester.step()
    tester.expect("io_data", initialValue + 1)
    println(s"m.data ${tester.peek("m.data")}")
    tester.step()
    tester.expect("io_data", initialValue + 2)
    println(s"m.data ${tester.peek("m.data")}")

    tester.report()
  }
}

/**
  * This is an implementation of a black box whose verilog is contained inline in AccumBlackBox, an instance of this
  * class will be placed into a black box factory so that it can be passed properly to the firrtl engine
  * @param name black box name
  */
class AccumFirrtlInterpreterBlackBox( val name : String) extends BlackBoxImplementation {

  var ns : BigInt = 0
  var ps : BigInt = 0
  var isInReset: Boolean = false

  def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "data" => Seq("clock", "reset")
      case _      => Seq.empty
    }
  }

  override def cycle(transition: Transition): Unit = {
    transition match {
      case PositiveEdge =>
        if(! isInReset) {
          ps = ns
          ns = ps + 1
        }
        // println(s"blackbox:$name ps $ps ns $ns")
      case _ =>
        // println(s"not positive edge, not action for cycle in $name")
    }
  }

  def execute(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    isInReset = inputValues.last != BigInt(0)
    ps
  }
}

/**
  * The factor that will provide firrtl access to the implementations
  */
class AccumBlackBoxFactory extends BlackBoxFactory {

  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "AccumBlackBox" => Some(add(new AccumFirrtlInterpreterBlackBox(instanceName)))
      case _               => None
    }
  }
}