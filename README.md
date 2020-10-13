Treadle -- A Chisel/Firrtl Execution Engine
==================

---

[![Join the chat at https://gitter.im/freechipsproject/firrtl](https://badges.gitter.im/freechipsproject/firrtl.svg)](https://gitter.im/freechipsproject/firrtl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
![Test](https://github.com/freechipsproject/treadle/workflows/Test/badge.svg)

**Treadle** is an experimental circuit simulator that executes low Firrtl IR.
It is based on earlier work on the [firrtl_interpreter](https://github.com/freechipsproject/firrtl-interpreter)
It will be one of the standard back-ends available as part of
the [chisel-testers](https://github.com/ucb-bar/chisel-testers.git) project, 
and thus one of the tools in the
[freechipsproject/chisel3](https://github.com/freechipsproject) hardware synthesis toolbox. 
This project provides a test harness supporting a peek, poke expect model.  
It also provides a interactive simulator shell or repl (see treadle.sh) that allows fine grained incremental
execution of a circuit. 
In combination with a scala debugger such as Eclipse or IntelliJ it can be a very powerful way of analyzing problematic
behavior.

[Chisel3](https://github.com/ucb-bar/chisel3.git) is a high-level functional circuit generator. It produces **Flexible Intermediate
Representation for RTL** or **FIRRTL**.  The [Firrtl](https://github.com/ucb-bar/firrtl.git) project parses and
transforms firrtl.  It also provides mechanisms for emitting verilog, for processing by downstream toolchains.
**Treadle** parses and execute the LoFirrtl subset of Firrtl. **Treadle** has a short spin up time and is close to
the performance of verilator simulations.
It can be useful for an initial debugging of Chisel circuits and is also used for other forms of circuit analysis.

## Using Treadle
### Attach it to your project
If you are using the [freechipsproject/chisel-testers](https://github.com/freechipsproject/chisel-testers) you will
have access to **Treadle** through it's dependency declarations.

If chisel-testers is not part of your tool chain then you must add the dependency explicitly.
To do so, in your project `build.sbt` add a dependency on
```
"edu.berkeley.cs" %% "treadle" % "1.1-SNAPSHOT"
```
There are a number of different ways to specify this dependency in the build.sbt file. If you have based your circuit on the 
[Chisel-template](https://github.com/freechipsproject/chisel-template.git) the addition should look like
```scala
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "edu.berkeley.cs" %% "chisel-iotesters" % "1.0",
  "edu.berkeley.cs" %% "treadle" % "1.1-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4")
```
for other usage consult **sbt** documentation

### Use the Tester Metaphor
The easiest way to invoke the interpreter is through a test based harness. The InterpretiveTester is very similar to the chisel
ClassicTester, it's api consists of poke, peek and expect statements. Here is an example of a GCD Circuit

```scala
import chisel._
import treadle.TreadleTester
import org.scalatest.{Matchers, FlatSpec}

object GCDCalculator {
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while(y > 0 ) {
      if (x > y) {
        x -= y
      }
      else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }
}

class GCD extends Module {
  val io = IO(new Bundle {
    val a  = Input(UInt(16.W))
    val b  = Input(UInt(16.W)))
    val e  = Input(Bool())
    val z  = Output(UInt(16.W))
    val v  = Output(Bool())
  })
  val x  = Reg(UInt())
  val y  = Reg(UInt())
  when(x > y) { x := x - y }
    .elsewhen(x <= y) { y := y - x }
  when (io.e) { x := io.a; y := io.b }
  io.z := x
  io.v := y === UInt(0)
}

class TreadleUsageSpec extends FlatSpec with Matchers {

  "GCD" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new GCD)

    val tester = TreadleTester(s)

    for {
      i <- 1 to 100
      j <- 1 to 100
    } {
      tester.poke("io_a", i)
      tester.poke("io_b", j)
      tester.poke("io_e", 1)
      tester.step()
      tester.poke("io_e", 0)

      var cycles = 0
      while (tester.peek("io_v") != BigInt(1)) {
        tester.step()
        cycles += 1
      }
      tester.expect("io_z", BigInt(GCDCalculator.computeGcd(i, j)._1))
      // uncomment the println to see a lot of output
      // println(f"GCD(${i}%3d, ${j}%3d) => ${interpretiveTester.peek("io_z")}%3d in $cycles%3d cycles")
    }
    tester.report()
  }
}
```

### Style conventions ScalaFmt
Treadle is the first repo in the chisel family to use the [ScalaFmt](https://scalameta.org/scalafmt/) code formatter. 
The plan going forward from 12/9/2019 is that all Scala code in PRs to Treadle after that date must be formatted using
the specification in the `.scalafmt.conf` file. Doing the formatting is simple and can be done via IntelliJ or
`sbt`.
More details can be found on the link above. 
For the present we are also interested in comments on the formatting decisions we have made.
Keep in mind that there is no set of rules that will satisfy everyone.

### About ports and names
The firrtl transformations that result in LoFirrtl alter the names of ports.
What would be io.a becomes io_a and so forth.  
************************************************
  
Adding Line Coverage to Treadle  
---------------
The idea was to modify the treadle tester so that it would make certain modifications to the given FIRRTL source code in order to track the line coverage. Once that was done, we needed to check these additional ports and compile the results in a line coverage report. The modifications related to this work can be found [here](https://github.com/chisel-uvm/treadle/blob/master/src/main/scala/treadle/coverage/CoverageParser.scala).  
  
### Modifying the input FIRRTL code  
The first thing that needed to be done was to add ports to the given design. This is done depending on the amount of multiplexers that are in the design, since any code that isn't in a multiplexer is executed by default. The idea would thus be to do the following:  
- Parse the input FIRRTL code to find:  
  1. The location of the i/o declaration  
  2. The locations of every multiplexer in the design  
  3. The total number of multiplexers  
- Add additional _coverage validator_ outputs in the i/o declaration part of the code. The idea is to add 2 additional 1-bit ports per multiplexer.  
- At each multiplexer use in the FIRRTL source, output the path taken to 2 coverage validators as follows:  
  ```scala  
  out_a <= mux(cond, in_a, in_b)  //Example use of a multiplexer  
  io_cov_valid_1 <= mux(cond, 1, 0)  
  io_cov_valid_2 <= mux(cond, 0, 1)  //Use of coverage validators to keep track of the path that was taken  
  ```    
    
These are all of the modifications that are needed to keep track of line coverage during a simulation.  
  
### Generating a Coverage report  
The main work here was interpreting the meaning of the outputs added to the FIRRTL source. The idea here was to do a second pass of the coverage parser once a given test was done. The big difference now is that we have access to the values of the different _coverage validators_, which allows us to know which multiplexer paths were taken during the different tests. So we can compile the data retrieved from the _coverage validators_ througout an entire test suite to find out which paths were taken and which were skipped. The results are then shown in a coverage report that has the following structure:  
```  
COVERAGE: 50.0% of multiplexer paths tested
COVERAGE REPORT:

+ circuit Test_1 :
+   module Test_1 :
+     input in$a : UInt<1>
+     input in$b$0 : UInt<2>
+     input in$b$1 : UInt<2>
+     input clock : Clock
+     output io_cov_valid_0 : UInt<1>
+     output io_cov_valid_1 : UInt<1>
+     output out : UInt<2>
+   
+     io_cov_valid_0 <= in$a
-     io_cov_valid_1 <= mux(in$a, UInt<1>("h0"), UInt<1>("h1"))
+     out <= mux(in$a, in$b$0, in$b$1)  
```
Here the first line gives a general percentage of how many of the possible control paths were taken and then the `COVERAGE REPORT` shows a modified version of the FIRRTL source with coverage prefixes added to each line:  
- `+` means that the line was covered by a test in the test suite.  
- `-` means that the line was missed during all of the test suite's tests.  
  
#### Interpreting the Coverage Report  
What this report tells us (in the above example) is that one of our artificially added FIRRTL lines wasn't covered. This information can be used to deduce which path of our multiplexer was taken. For example, in the above COVERAGE REPORT:  
```
+     io_cov_valid_0 <= in$a
-     io_cov_valid_1 <= mux(in$a, UInt<1>("h0"), UInt<1>("h1"))
+     out <= mux(in$a, in$b$0, in$b$1)  
```  
Actually tells us that the case where `in$a == 0` was never tested and thus one of our potential outputs was never tested.  
  
### Mapping loFIRRTL to Chisel  
For now, the coverage report isn't very interesting, since it's giving us information about a Intermediate Representation of our original Chisel code. A more interesting report would contain the same line coverage information, but shown directly in our original source description. The main problem about this is that Treadle actually functions using a low-level FIRRTL representation of our original source code and contains no direct reference to the original Chisel code, appart from a few `source locators` that can be found for lines that don't contain multiplexers and map them back to the source Chisel code.   
   
***Future work*** on this part of the project could thus be find a way to reconstruct the original Chisel source using the source locators and some smart guessing. A better version of the coverage report would thus be:  
```scala
ORIGINAL REPORT:

+ circuit Test_1 :
+   module Test_1 :
+     input in$a : UInt<1>
+     input in$b$0 : UInt<2>
+     input in$b$1 : UInt<2>
+     input clock : Clock
+     output io_cov_valid_0 : UInt<1>
+     output io_cov_valid_1 : UInt<1>
+     output out : UInt<2>
+   
+     io_cov_valid_0 <= in$a
-     io_cov_valid_1 <= mux(in$a, UInt<1>("h0"), UInt<1>("h1"))
+     out <= mux(in$a, in$b$0, in$b$1)  
```  
```scala
NEW REPORT:

+ class Test_1 extends Module {
+    val io = IO(new Bundle {
+        val a = Input(UInt(1.W))
+        val b = Input(Vec(2, UInt(2.W)))
+        val out = Output(UInt(2.W))  
+    })
+    when(io.a) {
+        out := io.b(0)
-   }.otherwise {
-        out := io.b(1)
+    }
+ }
```
