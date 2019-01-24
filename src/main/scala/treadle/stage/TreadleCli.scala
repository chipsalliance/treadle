// See LICENSE for license details.

package treadle.stage

import firrtl.options.Shell

trait TreadleCli { this: Shell =>
  parser.note("Treadle Front End Options")

  val commandLineAnnotations = Seq(
    SetVerbose,
    WriteVcd,
  )
  commandLineAnnotations.foreach(_.addOptions(parser))
}
