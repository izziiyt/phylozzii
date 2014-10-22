import math.{abs,exp}

object DoubleChecker {
  def apply(x:Double,y:Double,p:Double = exp(-10)):Boolean = {
    val tmp = abs(x - y)
    if(tmp < p) true else false
  }
}
