// See LICENSE for license details.

package treadle

import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

//scalastyle:off magic.number
case class TreadleOptions(
    writeVCD           : Boolean              = false,
    vcdShowUnderscored : Boolean              = false,
    setVerbose         : Boolean              = false,
    setOrderedExec     : Boolean              = false,
    allowCycles        : Boolean              = false,
    randomSeed         : Long                 = System.currentTimeMillis(),
    blackBoxFactories  : Seq[BlackBoxFactory] = Seq.empty,
    maxExecutionDepth  : Long                 = Int.MaxValue,
    showFirrtlAtLoad   : Boolean              = false,
    lowCompileAtLoad   : Boolean              = true,
    validIfIsRandom    : Boolean              = false,
    rollbackBuffers    : Int                  = 4,
    clockName          : String               = "clock",
    resetName          : String               = "reset",
    symbolsToWatch:    Seq[String]            = Seq.empty
  )
  extends firrtl.ComposableOptions {

  def vcdOutputFileName(optionsManager: ExecutionOptionsManager): String = {
    if(writeVCD) {
      s"${optionsManager.getBuildFileName("vcd")}"
    }
    else {
      ""
    }
  }
}

trait HasInterpreterOptions {
  self: ExecutionOptionsManager =>

  var treadleOptions = TreadleOptions()

  parser.note("firrtl-engine-options")

  parser.opt[Unit]("fint-write-vcd")
    .abbr("fiwv")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(writeVCD = true)
    }
    .text("writes vcd execution log, filename will be base on top")

  parser.opt[Unit]("fint-vcd-show-underscored-vars")
    .abbr("fivsuv")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(vcdShowUnderscored = true)
    }
    .text("vcd output by default does not show var that start with underscore, this overrides that")

  parser.opt[Unit]("fint-verbose")
    .abbr("fiv")
    .foreach {_ =>
      treadleOptions = treadleOptions.copy(setVerbose = true)
    }
    .text("makes engine very verbose")

  parser.opt[Unit]("fint-ordered-exec")
    .abbr("fioe")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(setOrderedExec = true)
    }
    .text("operates on dependencies optimally, can increase overhead, makes verbose mode easier to read")

  parser.opt[Unit]("fr-allow-cycles")
    .abbr("fiac")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(allowCycles = true)
    }
    .text(s"allow combinational loops to be processed, though unreliable, default is ${treadleOptions.allowCycles}")

  parser.opt[Long]("fint-random-seed")
    .abbr("firs")
      .valueName("<long-value>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(randomSeed = x)
    }
    .text("seed used for random numbers generated for tests and poison values, default is current time in ms")

  parser.opt[Unit]("show-firrtl-at-load")
    .abbr("fisfas")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(showFirrtlAtLoad = true)
    }
    .text("compiled low firrtl at firrtl load time")

  parser.opt[Unit]("dont-run-lower-compiler-on-load")
    .abbr("filcol")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(lowCompileAtLoad = false)
    }
    .text("run lowering compiler when firrtl file is loaded")

  parser.opt[Unit]("validif-random")
    .abbr("fivir")
    .foreach { _ =>
      treadleOptions = treadleOptions.copy(validIfIsRandom = true)
    }
    .text("validIf returns random value when condition is false")

  parser.opt[Int]("fint-rollback-buffers")
    .abbr("firb")
    .valueName("<int-value>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(rollbackBuffers = x)
    }
    .text("number of rollback buffers, 0 is no buffers, default is 4")

  parser.opt[String]("fint-clock-name")
    .abbr("ficn")
    .valueName("<string>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(clockName = x)
    }
    .text("name of default clock")

  parser.opt[String]("fint-reset-name")
    .abbr("firn")
    .valueName("<string>")
    .foreach { x =>
      treadleOptions = treadleOptions.copy(resetName = x)
    }
    .text("name of default reset")
}

object Driver {

  def execute(firrtlInput: String, optionsManager: InterpreterOptionsManager): Option[TreadleTester] = {
    val tester = new TreadleTester(firrtlInput, optionsManager)
    Some(tester)
  }

  def execute(args: Array[String], firrtlInput: String): Option[TreadleTester] = {
    val optionsManager = new InterpreterOptionsManager

    if (optionsManager.parser.parse(args)) {
      execute(firrtlInput, optionsManager)
    } else {
      None
    }
  }
}

class InterpreterOptionsManager extends ExecutionOptionsManager("engine") with HasInterpreterSuite

trait HasInterpreterSuite extends ExecutionOptionsManager with HasFirrtlOptions with HasInterpreterOptions {
  self : ExecutionOptionsManager =>
}
