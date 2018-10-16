// See LICENSE for license details.

package treadle.executable

/**
  * Created by chick on 4/21/16.
  */
case class TreadleException(message: String) extends Exception(message)

case class StopException(message: String) extends Exception(message)


