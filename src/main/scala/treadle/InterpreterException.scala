// See LICENSE for license details.

package treadle

/**
  * Created by chick on 4/21/16.
  */
class InterpreterException(message: String) extends Exception(message)
object InterpreterException {
  def apply(message: String): InterpreterException = new InterpreterException(message: String)
}

class StopException(message: String) extends Exception(message)
object StopException {
  def apply(message: String): StopException = new StopException(message: String)
}


