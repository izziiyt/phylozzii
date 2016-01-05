import java.io.{File, PrintWriter, OutputStream}
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.math._

package object util {

  def toTSV(arg:DenseVector[Double]):String = arg.toArray.mkString("\t")

  def doubleEqual(x:Double,y:Double,th:Double = 1.0E-14):Boolean = abs(x - y) < th

  def doubleEqual(x:DenseVector[Double],y:DenseVector[Double],th:Double):Boolean = doubleEqual(x.toArray,y.toArray,th)

  def doubleEqual(x:DenseMatrix[Double],y:DenseMatrix[Double],th:Double):Boolean = doubleEqual(x.toArray,y.toArray,th)

  def doubleEqual(x:Seq[Double],y:Seq[Double],th:Double):Boolean = (x,y).zipped.forall{(i,j) => doubleEqual(i,j,th)}

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

  def argmax[T](args:Seq[T], f:T => Double): T =
    args.foldLeft(args.head, Double.NegativeInfinity){
      case (m, a) =>
        val tmp = f(a)
        if(m._2 < tmp) (a, tmp) else m
    }._1

}
