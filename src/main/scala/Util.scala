import java.io.{OutputStream, PrintWriter}
import scala.io.Source
import scala.math.{abs,max,pow}

object Util {

  val EPSILON = 0.00001
  //.al file name to col alignments.
  def getAlignments(al:String):Array[Array[Char]] = {
    val source = Source.fromFile(al)
    val lines = source.getLines()
    val cols = lines.map(l => l.split(" ").map(_.toInt.toChar)).toArray
    source.close()
    cols
  }

  def doubleChecker(x:Double,y:Double,sig:Double=EPSILON):Boolean = abs(x - y) < sig * abs(max(x,y))

  def printExecutionTime[T](proc: => T,txt:String,os:OutputStream=System.out) = {
    val start = System.currentTimeMillis
    val result = proc
    val writer = new PrintWriter(os)
    writer.println(txt + " : " + (System.currentTimeMillis - start) + "msec")
    writer.flush()
    result
  }

}
