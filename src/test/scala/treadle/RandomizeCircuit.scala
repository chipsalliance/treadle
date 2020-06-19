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

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import logger.{LazyLogging, LogLevel, Logger}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle.chronometry.Timer

import scala.util.Random

class RandomizeCircuit extends AnyFreeSpec with Matchers with LazyLogging {
  val t = new Timer()

  def method1: Unit = {
    Random.setSeed(System.currentTimeMillis())
    t("method1") {
      val a = Array.tabulate(1000) { i =>
        Random.nextInt()
      }
      val sum = a.sum
      println(s"method1 sum $sum")
    }
  }

  def method2: Unit = {
    t("method2") {
      val a = Array.tabulate(1000) { i =>
        Random.setSeed(i)
        Random.nextInt()
      }
      val sum = a.sum
      println(s"method2 $sum")
    }
  }

  "random numbers should behave properly" in {
    method1
    method2
    method1
    method2
    t.clear()
    for (i <- 0 to 10) {
      method1
      method2
    }
    println(t.report())
  }

  "circuits can be randomized" in {
    val input =
      s"""
         |circuit ToBeRandomized :
         |  module ToBeRandomized :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io_r_data : UInt<8>
         |    input  io_r_addr : UInt<3>
         |    input  io_r_en   : UInt<1>
         |    input  io_w_data : UInt<8>
         |    input  io_w_addr : UInt<3>
         |    input  io_w_en   : UInt<1>
         |    input  io_w_mask : UInt<1>
         |    input  io_in_a   : UInt<16>
         |    output io_out_a  : UInt<16>
         |
         |    mem m :
         |      data-type => UInt<8>
         |      depth => 5
         |      read-latency => 1
         |      write-latency => 1
         |      reader => r
         |      writer => w
         |
         |    io_r_data   <= m.r.data
         |    m.r.addr    <= io_r_addr
         |    m.r.en      <= io_r_en
         |    m.r.clk     <= clock
         |
         |    m.w.data <= io_w_data
         |    m.w.addr <= io_w_addr
         |    m.w.en   <= io_w_en
         |    m.w.mask <= io_w_mask
         |    m.w.clk  <= clock
         |
         |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt(1)))
         |    reg reg2 : UInt<16>, clock with : (reset => (reset, UInt(2)))
         |    reg reg3 : UInt<16>, clock with : (reset => (reset, UInt(3)))
         |    reg reg4 : UInt<16>, clock with : (reset => (reset, UInt(4)))
         |
         |    reg reg11 : UInt<16>, clock with : (reset => (reset, UInt(11)))
         |    reg reg12 : UInt<16>, clock with : (reset => (reset, UInt(12)))
         |
         |    reg1 <= io_in_a
         |    reg2 <= add(io_in_a, io_in_a)
         |    reg3 <= add(io_in_a, UInt(1))
         |    reg4 <= add(io_in_a, UInt(17))
         |    node dog = and(reg1, reg2)
         |    node cat = and(reg3, reg4)
         |    reg11 <= dog
         |    reg12 <= cat
         |    node rat = xor(reg11, reg12)
         |    io_out_a <= rat
         |
         |
         |""".stripMargin

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(input),
        WriteVcdAnnotation
      )
    )
    // Logger.setLevel(tester.engine.getClass, LogLevel.Info)
    tester.poke("io_in_a", 7)
    tester.step(10)

    tester.randomize()

    tester.step(10)

    tester.reset(30)
    tester.step(10)

    tester.randomize()
    tester.step(10)

    tester.finish

    // GO AND LOOK AT VCD
  }
}
