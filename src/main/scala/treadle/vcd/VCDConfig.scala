// See LICENSE for license details.

package treadle.vcd

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasScoptOptions, OptionsView, RegisteredLibrary}
import scopt.OptionParser

case class VCDConfig(
  vcdSourceName:       String = "",
  vcdTargetName:       String = "",
  startScope:          String = "",
  renameStartScope:    String = "",
  varPrefix:           String = "",
  newVarPrefix:        String = ""
)

object VcdConfigViewer {
  implicit object VcdConfigView extends OptionsView[VCDConfig] {
    def view(options: AnnotationSeq): Option[VCDConfig] = {
      val executionOptions = options.foldLeft(VCDConfig()) { (previousOptions, annotation) =>
        annotation match {
          case VcdSourceNameAnnotation(name)       => previousOptions.copy(vcdSourceName = name)
          case VcdTargetNameAnnotation(name)       => previousOptions.copy(vcdTargetName = name)
          case VcdStartScopeAnnotation(name)       => previousOptions.copy(startScope = name)
          case VcdRenameStartScopeAnnotation(name) => previousOptions.copy(renameStartScope = name)
          case VcdVarPrefixAnnotation(name)        => previousOptions.copy(varPrefix = name)
          case _ => previousOptions
        }

      }
      Some(executionOptions)
    }
  }
}

sealed trait VcdConfigOption extends HasScoptOptions

case class VcdSourceNameAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-start-scope")
    .abbr("vcd-sf")
    .action( (name, c) => c :+ VcdSourceNameAnnotation(name) )
    .unbounded()
    .text("vcd source file")
}

case class VcdTargetNameAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-target-file")
    .abbr("vcd-tf")
    .action( (name, c) => c :+ VcdTargetNameAnnotation(name) )
    .unbounded()
    .text("vcd target file")
}

case class VcdStartScopeAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-start-scope")
    .abbr("vcd-ss")
    .action( (name, c) => c :+ VcdStartScopeAnnotation(name) )
    .unbounded()
    .text("vcd scope to start at")
}

case class VcdRenameStartScopeAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-rename-start-scope")
    .abbr("vcd-rss")
    .action( (name, c) => c :+ VcdRenameStartScopeAnnotation(name) )
    .unbounded()
    .text("vcd rename starting scope to this")
}

case class VcdVarPrefixAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-var-prefix")
    .abbr("vcd-vp")
    .action( (name, c) => c :+ VcdVarPrefixAnnotation(name) )
    .unbounded()
    .text("find vars with this prefix")
}

case class VcdNewVarPrefixAnnotation(name: String = "") extends NoTargetAnnotation with VcdConfigOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("vcd-new-var-prefix")
    .abbr("vcd-nvp")
    .action( (name, c) => c :+ VcdNewVarPrefixAnnotation(name) )
    .unbounded()
    .text("change vars prefix vars to this")
}

object TreadleReplLibrary extends RegisteredLibrary {
  override def name: String = "vcd-replay"

  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      VcdSourceNameAnnotation(),
      VcdTargetNameAnnotation(),
      VcdStartScopeAnnotation(),
      VcdRenameStartScopeAnnotation(),
      VcdVarPrefixAnnotation(),
      VcdNewVarPrefixAnnotation()
    )

    seq.foreach(_.addOptions(parser))
  }
}
