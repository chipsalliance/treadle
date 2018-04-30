// See LICENSE for license details.

package treadle.vcd

import firrtl.ExecutionOptionsManager
import firrtl.annotations.{Annotation, SingleStringAnnotation}

sealed trait VcdOption { self: Annotation => }
case class VcdSourceNameAnnotation(value: String) extends SingleStringAnnotation with VcdOption
case class VcdTargetNameAnnotation(value: String) extends SingleStringAnnotation with VcdOption
case class StartScopeAnnotation(value: String) extends SingleStringAnnotation with VcdOption
case class RenameStartScopeAnnotation(value: String) extends SingleStringAnnotation with VcdOption
case class VarPrefixAnnotation(value: String) extends SingleStringAnnotation with VcdOption
case class NewVarPrefixAnnotation(value: String) extends SingleStringAnnotation with VcdOption

case class VCDConfig(
  vcdSourceName:       Option[String] = None,
  vcdTargetName:       Option[String] = None,
  startScope:          Option[String] = None,
  renameStartScope:    Option[String] = None,
  varPrefix:           Option[String] = None,
  newVarPrefix:        Option[String] = None )


trait HasVCDConfig {
  self: ExecutionOptionsManager =>

  lazy val vcdConfig: VCDConfig = options
    .collect{ case a: VcdOption => a }
    .foldLeft(VCDConfig())( (v, x) =>
      x match {
        case VcdSourceNameAnnotation(n)    => v.copy(vcdSourceName = Some(n))
        case VcdTargetNameAnnotation(n)    => v.copy(vcdTargetName = Some(n))
        case StartScopeAnnotation(s)       => v.copy(startScope = Some(s))
        case RenameStartScopeAnnotation(r) => v.copy(renameStartScope = Some(r))
        case VarPrefixAnnotation(p)        => v.copy(varPrefix = Some(p))
        case NewVarPrefixAnnotation(p)     => v.copy(newVarPrefix = Some(p))
      }
    )

  parser.note("vcd")

  parser.arg[String]("<input-vcd-file>...")
    .required()
    .action( (x, a) => a :+ VcdSourceNameAnnotation(x) )
    .text("name of input vcd file")

  parser.arg[String]("<output-vcd-file>...")
    .optional()
    .action( (x, a) => a :+ VcdTargetNameAnnotation(x) )
    .text("name of output vcd file (optional)")

  parser.opt[String]("start-scope")
    .abbr("vss")
    .action( (x, a) => a :+ StartScopeAnnotation(x) )
    .text("starts saving information at specified scope")

  parser.opt[String]("rename-start-scope")
    .abbr("vrss")
    .action( (x, a) => a :+ RenameStartScopeAnnotation(x) )
    .text("rename startScope to this")

  parser.opt[String]("<retain-vars-with-prefix>...")
    .abbr("vrp")
    .action( (x, a) => a :+ VarPrefixAnnotation(x) )
    .text("only vars that start with prefix will be kept")

  parser.opt[String]("<new-var-prefix>...")
    .abbr("vnvp")
    .action( (x, a) => a :+ NewVarPrefixAnnotation(x) )
    .text("re-prefix vars with this string")
}

class VCDOptionsManager(args: Array[String]) extends ExecutionOptionsManager("vcd", args) with HasVCDConfig
