// SPDX-License-Identifier: Apache-2.0

package treadle.executable

import firrtl.ir._

/** Created by chick on 4/21/16.
  */
case class TreadleException(message: String) extends Exception(message)

case class StopException(
  stopValue: Int,
  stopName:  String,
  stopInfo:  Info)
    extends Exception {
  override def getMessage: String = {
    val state = if (stopValue == 0) { s"Stopped" }
    else { "Failure Stop" }
    val where = stopInfo match {
      case NoInfo => ""
      case info   => s" at $info"
    }
    s"$state:$stopName:($stopValue)$where"
  }

  def message: String = getMessage
}
