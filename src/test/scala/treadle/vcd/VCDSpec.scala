// See LICENSE for license details.

package treadle.vcd

import treadle.{TreadleOptionsManager, TreadleTester, VcdReplayTester, VcdReplayTesterOptions}
import firrtl.CommonOptions
import firrtl.util.BackendCompilationUtilities
import java.io.File

import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number
class VCDSpec extends FlatSpec with Matchers with BackendCompilationUtilities {
  private def getVcd = {
    VCD("test_circuit")
  }

  behavior of "vcd"

  it should "be able to generate unique ids " in {
    val vcd = getVcd

    val ids = new collection.mutable.HashSet[String]
    for (i <- 0 to 1000) {
      val id = vcd.getIdString(i)

      ids.contains(id) should be(false)
      ids += id

      id.forall { c => c.toInt >= 33 && c.toInt <= 126 } should be(true)
    }
  }

  it should "allow add wires" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    vcd.wires.contains("bob") should be(true)
    vcd.wires.contains("carol") should be(true)
    vcd.wires.contains("ted") should be(true)

    vcd.wires.contains("alice") should be(false)
  }

  it should "ignore calls to wire changed when value has not changed" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    // time starts at -1 to support initialized values
    vcd.incrementTime()
    for(i <- 0 to 10) {
      vcd.wireChanged("bob", i)
      vcd.wireChanged("carol", i / 2)
      vcd.wireChanged("ted", i / 4)
      vcd.incrementTime()
    }

    vcd.valuesAtTime(1).size should be (3)
    vcd.valuesAtTime(2).size should be (1)
    vcd.valuesAtTime(3).size should be (2)
    vcd.valuesAtTime(4).size should be (1)
    vcd.valuesAtTime(5).size should be (3)
    vcd.valuesAtTime(6).size should be (1)

    println(vcd.serialize)
  }

  it should "be able to serialize negative and positive values" in {
    val wire = Wire("testwire", "t", width = 4)
    val s = new StringBuilder
    for( i <- -8 to 7) {
      val change = Change(wire, i)
      val string = s"$i => ${change.serialize}"
      println(string)
      s ++= string + "\n"
    }
    s.toString().contains("-8 => b1000") should be (true)
    s.toString().contains("-1 => b1111") should be (true)
    s.toString().contains("0 => b0000") should be (true)
    s.toString().contains("1 => b0001") should be (true)
    s.toString().contains("7 => b0111") should be (true)
  }

  it should "serialize 1 bit numbers correctly" in {
    val c0 = Change(Wire("test1", "%", 1), 0)
    c0.serialize should be ("0%")

    val c1 = Change(Wire("test1", "%", 1), 1)
    c1.serialize should be ("1%")

    val c2 = Change(Wire("test1", "%", 1), -1)
    c2.serialize should be ("1%")
  }

  behavior of "VCD reader"

  it should "be able to read a file" in {
    val tempFile = File.createTempFile("GCD", ".vcd")
    tempFile.deleteOnExit()
    copyResourceToFile("/GCD.vcd", tempFile)
    val vcdFile = VCD.read(tempFile.getCanonicalPath)

    vcdFile.date should be ("2016-10-13T16:31+0000")
  }

  behavior of "vcd log containing negative numbers"

  it should "work correctly and be runnable from vcd output file" in  {

    val input =
      """
        |circuit Adder :
        |  module Adder :
        |    input clock : Clock
        |    input a : SInt<8>
        |    input b : SInt<8>
        |    output c : SInt<10>
        |
        |    c <= add(a, b)
      """.stripMargin

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(writeVCD = true)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }
    val engine = new TreadleTester(input, manager)
    engine.poke("a", -1)
    engine.peek("a") should be (BigInt(-1))
    engine.poke("b", -7)
    engine.peek("b") should be (BigInt(-7))

    engine.step()
    engine.peek("c") should be (BigInt(-8))

    engine.poke("a", 255)
    engine.peek("a") should be (BigInt(-1))
    engine.poke("b", 249)
    engine.peek("b") should be (BigInt(-7))

    engine.step()
    engine.peek("c") should be (BigInt(-8))
    engine.report()

  }

  behavior of "Using VCD output as a golden model test of a circuit"

  it should "be able to create a VCD then replay the VCD testing inputs" in {
    val stream = getClass.getResourceAsStream("/VcdAdder.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(writeVCD = true)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }

    val engine = new TreadleTester(input, manager)
    engine.step()
    engine.poke("io_a", 3)
    engine.poke("io_b", 5)
    engine.peek("io_a") should be (BigInt(3))
    engine.peek("io_b") should be (BigInt(5))

    engine.step()
    engine.peek("io_c") should be (BigInt(8))

    //    engine.poke("io_a", -1)
//    engine.poke("io_b", -7)
//    engine.peek("io_a") should be (BigInt(-1))
//    engine.peek("io_b") should be (BigInt(-7))
//
//    engine.step()
//    engine.peek("io_c") should be (BigInt(-8))

    engine.report()
  }

  behavior of "example from edysusanto"

  it should "align register updates with clock cycles" ignore {
    val input =
      """
        |circuit pwminCount :
        |  module pwminCount :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {testReg : UInt<4>}
        |
        |    clock is invalid
        |    reset is invalid
        |    io is invalid
        |    reg testReg : UInt<4>, clock with : (reset => (reset, UInt<1>("h00"))) @[RegisterVCDSpec.scala 30:24]
        |    node _T_6 = add(testReg, UInt<1>("h01")) @[RegisterVCDSpec.scala 31:22]
        |    node _T_7 = tail(_T_6, 1) @[RegisterVCDSpec.scala 31:22]
        |    testReg <= _T_7 @[RegisterVCDSpec.scala 31:11]
        |    io.testReg <= testReg @[RegisterVCDSpec.scala 32:14]
        |
      """.stripMargin

    // logger.Logger.setLevel(LogLevel.Debug)

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(writeVCD = true)
      commonOptions = CommonOptions(targetDirName = "test_run_dir/vcd_register_delay")
    }
    {
      val engine = new TreadleTester(input, manager)
      engine.setVerbose()
      engine.poke("reset", 0)

      engine.step(50)

      engine.report()
      engine.finish
    }

//    Thread.sleep(3000)

    val vcd = VCD.read("test_run_dir/vcd_register_delay/pwminCount.vcd")

    /* create an ordered indexed list of all the changes to testReg */
    val eventsOfInterest = vcd.valuesAtTime.filter {
      case (_, changeSet) =>
        changeSet.exists { change =>
          change.wire.name == "testReg"
        }
    }.toSeq.sortBy(_._1).map(_._2).toArray

    // at every step the io_testReg should be one cycle behind
    for(timeStep <- 4 to 24) {
      def getValue(step: Int, name: String): Int = {
        eventsOfInterest(step).find { change => change.wire.name == name}.head.value.toInt
      }

      getValue(timeStep, "testReg") should be (getValue(timeStep, "io_testReg"))
    }
  }

  behavior of "vcd replay spec"

  it should "replay a script and the treadle engine should match the vcd" in {
    val resourceName = "/VcdAdder.fir"
    val stream = getClass.getResourceAsStream(resourceName)
    val input = io.Source.fromInputStream(stream).mkString

    val manager = new TreadleOptionsManager {
      treadleOptions = treadleOptions.copy(
        writeVCD = true
      )
      commonOptions = CommonOptions(targetDirName = "test_run_dir/")
    }

    val resourceFileName = manager.targetDirName + resourceName
    copyResourceToFile(resourceName, new File(resourceFileName))
    val tester = new TreadleTester(input, manager)

    tester.poke("io_a", 3)
    tester.poke("io_b", 5)

    tester.step()

    tester.expect("io_c", 8)

    tester.report()
    tester.finish

    val replayManager = new VcdReplayTesterOptions() {

      goldenVcdOptions = goldenVcdOptions.copy(
        firrtlSourceName = resourceFileName,
        vcdSourceName = "test_run_dir/VcdAdder.vcd"
      )
    }
    val replayTester = new VcdReplayTester(replayManager)
    replayTester.run()

    replayTester.testSuccesses should be (7)
    replayTester.testFailures should be (0)
  }
}
