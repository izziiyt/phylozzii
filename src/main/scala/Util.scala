import scala.io.Source
import scala.math.{abs,exp}

object Util {
  def getAlignments(al:String):List[List[Char]] = {
    val source = Source.fromFile(al)
    val cols = for{
    //l <- source.getLines().take(100)
      l <- source.getLines()
      chrs = l.split(" ")
    } yield chrs.map(_.toInt.toChar).toList
    source.close()
    cols.toList
  }
  def doubleChecker(x:Double,y:Double,p:Double = exp(-10)):Boolean = {
    val tmp = abs(x - y)
    if(tmp < p) true else false
  }
}