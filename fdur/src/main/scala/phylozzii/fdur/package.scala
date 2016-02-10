package phylozzii

import java.io.{OutputStream, PrintWriter}

import alignment.Base
import biformat.MafIterator
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.LogDouble
import breeze.math.LogDouble._

import scala.annotation.tailrec

package object fdur {
  //import scala.language.reflectiveCalls
  //import scala.language.implicitConversions

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

  def readMaf(it: MafIterator, per: Int = 512): Array[List[Array[Base]]] = {
    val totalunit = it.reduceLeft { (n, u) => n + u }
    val bases = totalunit.seqs
    val tmp = div(bases, per)
    tmp
  }

  def div[T](seqs: List[Array[T]], size: Int): Array[List[Array[T]]] = {
    @tailrec
    def f(xs: List[Array[T]], ys: List[List[Array[T]]], index: Int): Array[List[Array[T]]] = {
      if (xs.head.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map { x => x.splitAt(index) }.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs, Nil, size)
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
