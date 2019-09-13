// See LICENSE for license details.

package treadle

import firrtl.FileUtils
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

class CacheSpec extends FreeSpec with Matchers {
  "low firrtl passes should not create combinational loops" in {
    val firrtl = FileUtils.getText("Cache.fir")

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtl)))
  }
}
