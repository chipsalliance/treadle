// See LICENSE for license details.

package treadle

import firrtl.FirrtlSourceAnnotation
import treadle.executable.TreadleException

object TreadleFactory {
  def apply(input: String, args: String*): TreadleTester = {
    Driver.execute(args.toArray, Seq(FirrtlSourceAnnotation(input))) match {
      case TreadleTesterCreated(tester) => tester
      case _ =>
        throw TreadleException(
          s"could not construct Treadle Tester for args (${args.mkString(" ")})"
        )
    }
  }
}
