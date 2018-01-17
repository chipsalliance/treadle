// See LICENSE for license details.

package treadle.vcd

import firrtl.ExecutionOptionsManager

case class VCDConfig(
                     vcdSourceName:       String = "",
                     vcdTargetName:       String = "",
                     startScope:          String = "",
                     renameStartScope:    String = "",
                     varPrefix:           String = "",
                     newVarPrefix:        String = "")
  extends firrtl.ComposableOptions {
}

trait HasVCDConfig {
  self: ExecutionOptionsManager =>

  var vcdConfig = VCDConfig()

  parser.note("vcd")

  parser.opt[String]("start-scope")
    .abbr("vss")
    .foreach { x => vcdConfig = vcdConfig.copy(startScope = x)}
    .text("starts saving information at specified scope")

  parser.opt[String]("rename-start-scope")
    .abbr("vrss")
    .foreach { x => vcdConfig = vcdConfig.copy(renameStartScope = x)}
    .text("rename startScope to this")

  parser.opt[String]("<retain-vars-with-prefix>...")
    .abbr("vrp")
    .foreach { x => vcdConfig = vcdConfig.copy(varPrefix = x) }
    .text("only vars that start with prefix will be kept")

  parser.opt[String]("<new-var-prefix>...")
    .abbr("vnvp")
    .foreach { x => vcdConfig = vcdConfig.copy(newVarPrefix = x) }
    .text("re-prefix vars with this string")

  parser.arg[String]("<input-vcd-file>...")
    .required()
    .foreach { x => vcdConfig = vcdConfig.copy(vcdSourceName = x) }
    .text("name of input vcd file")

  parser.arg[String]("<output-vcd-file>...")
    .optional()
    .foreach { x => vcdConfig = vcdConfig.copy(vcdTargetName = x) }
    .text("name of output vcd file (optional)")
}

class VCDOptionsManager extends ExecutionOptionsManager("vcd") with HasVCDConfig