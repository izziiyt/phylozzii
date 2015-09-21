import java.io.{PrintWriter, OutputStream}

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.LogDouble
import breeze.math.LogDouble._

import scala.annotation.tailrec
import scala.math._
import scala.util.Random

package object fdur {
  import alignment.Base

  type VD = DenseVector[Double]
  type MD = DenseMatrix[Double]
  type VL = DenseVector[LogDouble]
  type ML = DenseMatrix[LogDouble]

  val LogDoubleZero = SemiringLogDouble.zero

  def printExeTime[T](proc: => T,txt:String,os:OutputStream=System.out) = {
    val start = System.currentTimeMillis
    val result = proc
    val writer = new PrintWriter(os)
    writer.println(txt + "\t" + (System.currentTimeMillis - start))
    writer.flush()
    result
  }

  implicit def DenseMatrixDoubleExtra(x: DenseMatrix[Double]) = new {
    def toLogDouble: DenseMatrix[LogDouble] = x.map(_.toLogDouble)
  }

  implicit def DenseMatrixLogDoubleExtra(x: DenseMatrix[LogDouble]) = new {
    def value: DenseMatrix[Double] = x.map(_.value)
    def logValue: DenseMatrix[Double] = x.map(_.logValue)
  }

  implicit def DenseVectorDoubleExtra(x: DenseVector[Double]) = new {
    def toLogDouble: DenseVector[LogDouble] = x.map(_.toLogDouble)
  }

  implicit def DenseVectorLogDoubleExtra(x: DenseVector[LogDouble]) = new {
    def value: DenseVector[Double] = x.map(_.value)
    def logValue: DenseVector[Double] = x.map(_.logValue)
  }
}
