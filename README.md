Firrtl-Interpreter
==================

The firrtl interpreter is an experimental circuit simulator that executes low Firrtl IR.
It is one of the standard back-ends available as part of the [chisel-testers](https://github.com/ucb-bar/chisel-testers.git) project, 
and thus one of the tools in the [UCB-BAR/chisel](https://github.com/ucb-bar) hardware synthesis toolbox. 
This project provides a test harness supporting a peek, poke expect model.  
It also provides a interactive simulator shell or repl (see frepl.sh) that allows fine grained incremental execution of
 a circuit. 
In combination with a scala debugger such as Eclipse or IntelliJ it can be a very power way of analyzing problematic
behavior.  The interpreter has not yet been optimzed for performance and may be too slow for emulation of very complex
circuits.

[Chisel3](https://github.com/ucb-bar/chisel3.git) is a high-level functional circuit generator. It produces **Flexible Intermediate
Representation for RTL** or **FIRRTL**.  The [Firrtl](https://github.com/ucb-bar/firrtl.git) project parses and
transforms firrtl.  It also provides mechanisms for emitting verilog, for processing by downstream toolchains.
This interpreter parse and execute the LoFirrtl subset of Firrtl. The interpreter is useful for a initial debugging of Chisel circuits
and is also used for other forms of circuit analysis. 

## Using the interpreter
### Attach it to your project
Most Chisel development projects will have a an SBT file that describes dependencies. To access the firrtl-interpreter in your project
add a dependency on
```
"edu.berkeley.cs" %% "firrtl-interpreter" % "0.1-SNAPSHOT"
```
There are a number of different ways to specify this dependency in the build.sbt file. If you have based your circuit on the 
[Chisel-template](https://github.com/ucb-bar/chisel-template.git) the addition should look like
```scala
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "edu.berkeley.cs" %% "chisel-iotesters" % "1.0",
  "edu.berkeley.cs" %% "firrtl-interpreter" % "0.1-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4")
```
for other usage consult **sbt** documentation

### Use the tester metaphor
The easiest way to invoke the interpreter is through a test based harness. The InterpretiveTester is very similar to the chisel
ClassicTester, it's api consists of poke, peek and expect statements. Here is an example of a GCD Circuit

```scala
import Chisel._
import firrtl_interpreter.InterpretiveTester
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
  val io = new Bundle {
    val a  = UInt(INPUT,  16)
    val b  = UInt(INPUT,  16)
    val e  = Bool(INPUT)
    val z  = UInt(OUTPUT, 16)
    val v  = Bool(OUTPUT)
  }
  val x  = Reg(UInt())
  val y  = Reg(UInt())
  when   (x > y) { x := x - y }
  unless (x > y) { y := y - x }
  when (io.e) { x := io.a; y := io.b }
  io.z := x
  io.v := y === UInt(0)
}


class InterpreterUsageSpec extends FlatSpec with Matchers {

  "GCD" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new GCD)

    val tester = new InterpretiveTester(s)

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
The firrtl transformations that result in LoFirrtl alter the names of ports.  What would be io.a becomes io_a and so forth.

### Using the interpreter with a debugger
This section under development