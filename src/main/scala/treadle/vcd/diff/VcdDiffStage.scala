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

package treadle.vcd.diff

import firrtl.AnnotationSeq
import firrtl.options.{ProgramArgsAnnotation, Shell, Stage}
import treadle.vcd.VCD

trait VcdDiffCli { this: Shell =>
  parser.note("VCDDiff Command Line Options")
  Seq(
    DisplayRadix,
    V1StartTime,
    MaxDiffLines,
    TimeOffset,
    CompareWires,
    UnmatchedWires,
    DontDiffValues,
    IgnoreTempWires,
    WirePrefix1,
    WirePrefix2
  ).foreach(_.addOptions(parser))
}

class VcdDiffStage extends Stage {
  override val shell: Shell = new Shell("VCDDiff") with VcdDiffCli

  override def run(annotations: AnnotationSeq): AnnotationSeq = {

    val vcds = annotations.collect {
      case ProgramArgsAnnotation(fileName) =>
        VCD.read(fileName)
    }
    if (vcds.length != 2) {
      println("Error: Two files must be specifed for diff to run\nUsage: VCDDiff options <file1> <file2>")
      System.exit(1)
    }

    val vcdDiff = new VcdComparator(annotations)
    vcdDiff.compare(vcds.head, vcds.tail.head)

    annotations
  }
}
