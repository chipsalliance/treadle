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

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class RegisterCycleTest extends FreeSpec with Matchers {
  "cycle behavior test-only intepreter should not crash on various register init methods" - {
    "method 1" in {
      val input =
        """
          |circuit myModule :
          |  module mySubModule :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_In : UInt<8>
          |    output io_Out : UInt<8>
          |
          |    reg delayReg : UInt<8>, clock with :
          |      reset => (reset, UInt<8>("h0"))
          |    io_Out <= delayReg
          |    delayReg <= io_In
          |
          |  module myModule :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_In : UInt<8>
          |    output io_Out : UInt<8>
          |
          |    inst mySubModule_1 of mySubModule @[myModuleTest.scala 22:25]
          |    io_Out <= mySubModule_1.io_Out
          |    mySubModule_1.io_In <= io_In
          |    mySubModule_1.clock <= clock
          |    mySubModule_1.reset <= reset
          |
        """.stripMargin

      for (i <- 0 to 10) {
        println(s"experiment $i")
        scala.util.Random.setSeed(i.toLong)
        val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))

        tester.poke("reset", 1)
        tester.step()
        tester.poke("io_In", 1)
        tester.poke("reset", 0)
        tester.step()
        println(s"System state:")
        println(s"${tester.engine.header}")
        println(s"System state: ${tester.engine.dataInColumns}")
        tester.expect("io_Out", 1)
      }
    }

    "method 2" in {
      val input =
        """
          |circuit myModule :
          |  module mySubModule :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_In : UInt<8>
          |    output io_Out : UInt<8>
          |
          |    reg delayReg : UInt<8>, clock
          |    io_Out <= delayReg
          |    delayReg <= io_In
          |
          |  module myModule :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    input io_In : UInt<8>
          |    output io_Out : UInt<8>
          |
          |    inst mySubModule_1 of mySubModule @[myModuleTest.scala 22:25]
          |    io_Out <= mySubModule_1.io_Out
          |    mySubModule_1.io_In <= io_In
          |    mySubModule_1.clock <= clock
          |    mySubModule_1.reset <= reset
          |
        """.stripMargin

      val output = new ByteArrayOutputStream()
      Console.withOut(new PrintStream(output)) {
        val optionsManager = new TreadleOptionsManager
        optionsManager.parser.parse(Array("-tstw", "io_Out,mySubModule_1.io_Out"))

        val tester = TreadleTester(
          Seq(FirrtlSourceAnnotation(input), SymbolsToWatchAnnotation(Seq("io_Out", "mySubModule_1.io_Out")))
        )

        tester.poke("io_In", 1)
        tester.step(3)
        tester.expect("io_Out", 1)
      }

      output.toString.contains("io_Out <= 1") should be(true)
    }
  }

}
