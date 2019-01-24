// See LICENSE for license details.

package treadle

import firrtl.AnnotationSeq
import treadle.stage.TreadleOptions
import firrtl.options.OptionsView

package object stage {

  implicit object TreadleOptionsView extends OptionsView[TreadleOptions] {

    def view(options: AnnotationSeq): TreadleOptions = {
      options
              .collect { case a: TreadleOption => a }
              .foldLeft(new TreadleOptions()) { (c, x) =>
                x match {
                  case WriteVcd => c.copy(writeVcd = true)
                }
              }
    }
  }

}
