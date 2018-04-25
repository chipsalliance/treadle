// See LICENSE for license details.

package treadle.executable

case class ClockInfo(
  name          : String = ClockInfo.DefaultName,
  period        : Long   = ClockInfo.DefaultPeriod,
  initialOffset : Long   = ClockInfo.DefaultOffset
)

object ClockInfo {
  val DefaultName: String = "clock"
  val DefaultPeriod: Long = 10L
  val DefaultOffset: Long = 1L
}
