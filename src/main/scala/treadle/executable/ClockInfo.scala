// See LICENSE for license details.

package treadle.executable

case class ClockInfo(
  name          : String = ClockInfo.DefaultName,
  period        : Long   = ClockInfo.DefaultPeriod,
  initialOffset : Long   = ClockInfo.DefaultOffset
) {
  val upPeriod   : Long = period / 2
  val downPeriod : Long = period - upPeriod
}

object ClockInfo {
  /*
  These are the following settings that seem to parallel the default verilator settings.
   */
  val DefaultName: String = "clock"
  val DefaultPeriod: Long = 10L
  val DefaultOffset: Long = -4L
}
