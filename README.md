Treadle -- A Chisel/Firrtl Execution Engine
==================

**Treadle** is an experimental circuit simulator that executes low Firrtl IR.
It is based on earlier work on the [firrtl_interpreter](https://github.com/freechipsproject/firrtl-interpreter)
It will be one of the standard back-ends available as part of
the [chisel-testers](https://github.com/ucb-bar/chisel-testers.git) project, 
and thus one of the tools in the
[freechipsproject/chisel3](https://github.com/freechipsproject) hardware synthesis toolbox. 
This project provides a test harness supporting a peek, poke expect model.  
It also provides a interactive simulator shell or repl (see treadle.sh) that allows fine grained incremental
execution of a circuit. 
In combination with a scala debugger such as Eclipse or IntelliJ it can be a very power way of analyzing problematic
behavior.

[Chisel3](https://github.com/ucb-bar/chisel3.git) is a high-level functional circuit generator. It produces **Flexible Intermediate
Representation for RTL** or **FIRRTL**.  The [Firrtl](https://github.com/ucb-bar/firrtl.git) project parses and
transforms firrtl.  It also provides mechanisms for emitting verilog, for processing by downstream toolchains.
**Treadle** parses and execute the LoFirrtl subset of Firrtl. **Treadle** has a short spin up time and is close to 
the performance of verilator simulations.
It can useful for a initial debugging of Chisel circuits and is also used for other forms of circuit analysis. 

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

### About ports and names
The firrtl transformations that result in LoFirrtl alter the names of ports.
What would be io.a becomes io_a and so forth.
