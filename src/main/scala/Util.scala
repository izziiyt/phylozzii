import scala.io.Source
import scala.math.{abs,exp}

object Util {

  def getAlignments(al:String):Array[Array[Char]] = {
    val source = Source.fromFile(al)
    val lines = source.getLines()
    val cols = lines.map(l => l.split(" ").map(_.toInt.toChar)).toArray
    source.close()
    cols
  }

  def doubleChecker(x:Double,y:Double,p:Double = exp(-10)):Boolean = {
    val tmp = abs(x - y)
    if(tmp < p) true else false
  }

  def printExecutionTime[T](proc: => T,txt:String) = {
    val start = System.currentTimeMillis
    val x = proc
    println(txt + " : " + (System.currentTimeMillis - start) + "msec")
    x
  }

}