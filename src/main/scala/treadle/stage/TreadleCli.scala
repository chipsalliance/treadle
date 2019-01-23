// See LICENSE for license details.

package treadle.stage

import firrtl.options.Shell

trait TreadleCli { this: Shell =>
  parser.note("Treadle Front End Options")

  Seq(
    WriteVcd
  )
          .foreach(_.addOptions(parser))
}
