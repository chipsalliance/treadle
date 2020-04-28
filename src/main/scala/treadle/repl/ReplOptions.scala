/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.repl

import java.io.OutputStream

import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait ReplOption extends Unserializable { this: Annotation =>
}

case class OverrideOutputStream(outputStream: OutputStream) extends NoTargetAnnotation

case class DefaultFileNameWithOutSuffix(fileName: String)

/**
  * @param scriptName The name of a script file to load at runtime
  */
case class TreadleScriptFile(scriptName: String) extends NoTargetAnnotation with ReplOption

/**
  * Tells treadle load the specified script file, basically a text file of treadle repl commands
  */
case object TreadleScriptFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-script-file",
      shortOption = Some("tsf"),
      toAnnotationSeq = (s: String) => Seq(TreadleScriptFile(s)),
      helpText = "read a text file of treadle commands"
    )
  )
}

/**
  * Tells treadle to write a vcd file during simulation
  */
case object TreadleReplUseVcd extends NoTargetAnnotation with ReplOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-use-vcd-script",
      shortOption = Some("tuvs"),
      toAnnotationSeq = _ => Seq(TreadleReplUseVcd),
      helpText = "load vcd file as script, default is false"
    )
  )
}

/**
  * @param scriptName The name of a script file to load at runtime
  */
case class TreadleVcdScriptFileOverride(scriptName: String) extends NoTargetAnnotation with ReplOption

/**
  * Tells treadle to write a vcd file during simulation
  */
case object TreadleVcdScriptFileOverride extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-vcd-script-override",
      shortOption = Some("tvso"),
      toAnnotationSeq = (s: String) => Seq(TreadleVcdScriptFileOverride(s)),
      helpText = "file to use as vcd script, default is circuit name with .vcd suffix"
    )
  )
}

/**
  * @param format output format, d, x, or b
  */
case class TreadleReplDisplayFormat(format: String) extends NoTargetAnnotation with ReplOption

/**
  * Tells treadle to write a vcd file during simulation
  */
case object TreadleReplDisplayFormat extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-display-format",
      shortOption = Some("tdf"),
      toAnnotationSeq = (s: String) => Seq(TreadleReplDisplayFormat(s)),
      helpText = "how to display values d - decimal, x - hex, b - binary"
    )
  )
}

/**
  * Tells treadle to write a vcd file during simulation
  */
case object TreadleReplRunScriptAtStartup extends NoTargetAnnotation with ReplOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-run-script-on-startup",
      shortOption = Some("trsas"),
      toAnnotationSeq = _ => Seq(TreadleReplRunScriptAtStartup),
      helpText = "run script immediately on startup"
    )
  )
}
