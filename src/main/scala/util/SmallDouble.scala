/*package util

import breeze.numerics.{exp, log}

class SmallDouble protected (protected val logv: Double) {
  def nlogv:Double = exp(nlogv)
  def +(that: SmallDouble) = LogDouble()
  def *(that: SmallDouble) = SmallDouble(this.logv + that.logv)
}

object SmallDouble{
  def fromDouble(x: Double) = new LogDouble(log(x))
  def fromLDouble(x:Double) =
}
*/