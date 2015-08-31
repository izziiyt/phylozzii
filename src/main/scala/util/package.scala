import java.io.{File, PrintWriter, OutputStream}

import alignment.Base
import breeze.linalg.DenseVector

import scala.io.Source
import scala.math._

package object util {
  val EPSILON = 0.00001
  //.al file name to col alignments.
  def getAlignments(al:String):Array[Array[Base]] = {
    val source = Source.fromFile(al)
    val lines = source.getLines()
    val cols = lines.map(l => l.toCharArray.map(Base.fromChar)).toArray
    source.close()
    cols
  }

  def toTSV(arg:DenseVector[Double]):String = arg.foldLeft(""){(x,i) => x + "\t" + i}.toString.tail

  def doubleChecker(x:Double,y:Double,th:Double = 1.0E-14):Boolean = abs(x - y) < th

  def doubleChecker(x:DenseVector[Double],y:DenseVector[Double]):Boolean = doubleChecker(x.toArray,y.toArray)

  def doubleChecker(x:Seq[Double],y:Seq[Double]):Boolean = (x,y).zipped.forall{(i,j) => doubleChecker(i,j)}

  def printExecutionTime[T](proc: => T,txt:String,os:OutputStream=System.out) = {
    val start = System.currentTimeMillis
    val result = proc
    val writer = new PrintWriter(os)
    writer.println(txt + "\t" + (System.currentTimeMillis - start))
    writer.flush()
    result
  }

  def lsf(dir: String) : Seq[File] = {
    new File(dir).listFiles.flatMap {
      case f if f.isDirectory => lsf(f.getPath)
      case x => List(x)
    }
  }

  def argmax[T](args:Seq[T],f:T => Double): T = {
    var j = 0
    var m = Double.NegativeInfinity
    for(i <- args.indices;v = f(args(i));if v > m){m = v;j = i}
    args(j)
  }
}
