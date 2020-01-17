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

//
//treadle treadle
//
//import org.scalatest.{Matchers, FlatSpec}
//
//class SourceInfoSpec extends FlatSpec with Matchers {
//  behavior of "source information"
//
//  it should "be visible when logging and errors occur" in {
//    val stream = getClass.getResourceAsStream("/FullAdder.ir")
//    val input = io.Source.fromInputStream(stream).mkString
//
//    val f = ExecutionEngine(input)
//
//    f.evaluator.setVerbose(true)
//    f.cycle()
//    f.dependencyGraph.sourceInfo("a_and_b") should fullyMatch regex ".*FullAdder.scala 19:22.*"
//  }
//
//}
