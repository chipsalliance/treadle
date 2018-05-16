// See LICENSE for license details.

package treadle.executable

/**
  * Created by chick on 4/21/16.
  */
class TreadleException(message: String) extends Exception(message)
object TreadleException {
  def apply(message: String): TreadleException = new TreadleException(message: String)
}

class StopException(message: String) extends Exception(message)
object StopException {
  def apply(message: String): StopException = new StopException(message: String)
}


