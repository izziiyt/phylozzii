package util

import breeze.linalg.DenseVector
import breeze.math.LogDouble
import breeze.math.LogDouble.doubleExtra

class TraversedLDVector(val x:Vector[LogDouble]) extends LDVectorLike{
  import LDVector.LogDoubleZero
  def *(that: LDVector): LogDouble = {
    require(that.length == this.length)
    x.indices.foldLeft(LogDoubleZero){(n,i) => n + (that.x(i) * x(i))}
  }
}

class LDVector protected (val x:Vector[LogDouble]) extends LDVectorLike{
  def t: TraversedLDVector = new TraversedLDVector(x)
  def +(that: LDVector): LDVector = {
    require(this.length == that.length)
    val tmp = for(i <- indices) yield this.x(i) + that.x(i)
    LDVector(tmp.toVector)
  }
  def :*(that: LDVector): LDVector = {
    require(this.length == that.length)
    val tmp = for(i <- indices) yield this.x(i) * that.x(i)
    LDVector(tmp.toVector)
  }
  def toDenseVector: DenseVector[Double] = DenseVector(x.toArray.map(_.value))
}

trait LDVectorLike {
  def x: Vector[LogDouble]
  def length: Int = x.length
  def apply(i: Int): LogDouble = x(i)
  def indices = x.indices
}

object LDVector {

  val LogDoubleZero = 0.0 toLogDouble

  def zeros(n: Int): LDVector = new LDVector(Vector.fill(n)(LogDoubleZero))

  def ones(n: Int): LDVector = LDVector(Vector.fill(n)(0.0 asLogDouble))

  def apply(x: Vector[LogDouble]): LDVector = new LDVector(x)

  def apply(x: Array[LogDouble]): LDVector = LDVector(x.toVector)

  def apply(x: Array[Double]): LDVector = LDVector(x.map(_.toLogDouble))

  implicit def DenseVectorDoubleExtra(x: DenseVector[Double]) = new {
    def toLDVector: LDVector = LDVector(x.toArray.map(_.toLogDouble))

    def toLogDouble: DenseVector[LogDouble] = x.map(_.toLogDouble)
  }

  implicit def DenseVectorLogDoubleExtra(x: DenseVector[LogDouble]) = new {
    def value: DenseVector[Double] = x.map(_.value)
    def logValue: DenseVector[Double] = x.map(_.logValue)
  }

  def apply(xs: Double*): LDVector = LDVector(xs.map(_.toLogDouble).toVector)

}