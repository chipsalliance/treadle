// See LICENSE for license details.

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.{HasScoptOptions, RegisteredLibrary}
import scopt.OptionParser

class TreadleLibrary extends RegisteredLibrary {
  val name = "treadle"

  override def addOptions(p: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      WriteVcd,
      VcdShowUnderscored,
      SetVerbose,
      AllowCycles,
      RandomSeed,
      ShowFirrtlAtLoad,
      LowCompileAtLoad,
      ValidIfIsRandom,
      RollbackBuffers,
      ClockInfoList,
      ResetName,
      CallResetAtStartup,
      SymbolsToWatch
    )

    seq.foreach(_.addOptions(p))
  }
}
