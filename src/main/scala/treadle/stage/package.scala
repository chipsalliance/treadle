// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

package object stage {

  implicit object TreadleConfigView extends OptionsView[TreadleConfig] {

    def view(options: AnnotationSeq): TreadleConfig = {
      options
              .collect { case a: TreadleOption => a }
              .foldLeft(TreadleConfig()) { (c, x) =>
                x match {
                  case WriteVcd => c.copy(writeVcd = true)
                  case _ => c
                }
              }
    }
  }

}
