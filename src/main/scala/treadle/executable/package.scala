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

package treadle

import firrtl.ir.Info

package object executable {
  type Big = BigInt

  object Big {
    def apply(n: Int): Big = BigInt(n)
  }

  trait ExpressionResult

  type FuncInt = () => Int
  type FuncLong = () => Long
  type FuncBig = () => Big
  type FuncUnit = () => Unit

  trait Assigner {
    val symbol: Symbol
    val info:   Info
    def run: FuncUnit
    def render: String = symbol.render

    def setLeanMode(isLean: Boolean): Unit = {}

    private var verboseAssign: Boolean = false
    def isVerbose:             Boolean = verboseAssign
    def setVerbose(value: Boolean): Unit = {
      verboseAssign = value
    }

    private var renderAssign: Boolean = false
    def getRenderMode:        Boolean = renderAssign
    def setRender(value: Boolean): Unit = {
      renderAssign = value
    }
  }
}
