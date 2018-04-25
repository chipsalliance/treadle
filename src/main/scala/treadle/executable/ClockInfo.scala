// See LICENSE for license details.

package treadle.executable

/**
  * ClockInfo associates a clock with the given name and period and offset
  * The period is in an arbitrary number of ticks.  The VCD logger currently
  * sets these ticks to be nanosecond(ns).
  * The first up transition takes place after initialOffset ticks.
  * One or more clocks can be specified through the TreadleOptions clockInfo as a Seq of ClockInfo's
  * or from string command line based --fint-clock-info or -fici
  * which use the format clock-name[:period[:initial-offset] ]
  *
  * @param name           the signal name of a clock
  * @param period         how many ticks between rising edges of this clock
  * @param initialOffset  how many ticks before the first rising edge occurs, this value can be negative
  */
case class ClockInfo(
  name          : String = ClockInfo.DefaultName,
  period        : Long   = ClockInfo.DefaultPeriod,
  initialOffset : Long   = ClockInfo.DefaultOffset
)

/**
  * The default settings for a single clock are here.  Units are in arbitrary ticks
  */
object ClockInfo {
  val DefaultName: String = "clock"
  val DefaultPeriod: Long = 10L
  val DefaultOffset: Long = 1L
}
