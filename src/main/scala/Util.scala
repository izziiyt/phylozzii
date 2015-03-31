import breeze.linalg.DenseVector
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

  def toTSV(arg:DenseVector[Double]):String = arg.foldLeft(""){(x,i) => x + "\t" + i}.toString.tail

  def doubleChecker(x:Double,y:Double):Boolean = abs(x - y) < 1.0E-14
  //def doubleChecker(x:Double,y:Double,sig:Double=EPSILON):Boolean = abs(x - y) < sig * abs(max(x,y))

  def printExecutionTime[T](proc: => T,txt:String,os:OutputStream=System.out) = {
    val start = System.currentTimeMillis
    val result = proc
    val writer = new PrintWriter(os)
    writer.println(txt + "\t" + (System.currentTimeMillis - start))
    writer.flush()
    result
  }

}
