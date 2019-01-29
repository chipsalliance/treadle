// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

package object stage {

  implicit object TreadleConfigView extends OptionsView[TreadleOptions] {

    def view(options: AnnotationSeq): TreadleOptions = {
      options
              .collect { case a: TreadleOption => a }
              .foldLeft(TreadleOptions()) { (c, x) =>
                x match {
                  case WriteVcd => c.copy(writeVCD = true)
                  case _ => c
                }
              }
    }
  }

}
