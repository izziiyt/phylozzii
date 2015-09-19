package util

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.math.LogDouble
import breeze.math.LogDouble.doubleExtra
import breeze.math.LogDouble.SemiringLogDouble
import breeze.numerics.exp

trait Mymatrix {
  def x = Vector[LogDouble]
  def :*(that:Vector[LogDouble]): Mymatrix
  def +(that:Vector[LogDouble]): Mymatrix
  def *(that:Vector[LogDouble]): Mymatrix
  def +(v:LogDouble): Mymatrix
  def *(v:LogDouble): Mymatrix
  def +(v:Double): Mymatrix
  def *(v:Double): Mymatrix
}

class LDMatrix protected (val x: Vector[LogDouble], val nrow: Int, val ncol: Int) {
  require(x.length == nrow * ncol)

  def apply(i: Int, j: Int):LogDouble = {
    require(0 < i && i < nrow && 0 < j && j < ncol)
    x(i * ncol + j)
  }

  def :*(that:LDMatrix) = sequentialFunc(that,(x: LogDouble, y: LogDouble) => x * y)

  def +(that:LDMatrix) = sequentialFunc(that,(x: LogDouble, y: LogDouble) => x + y)

  protected def sequentialFunc(that:LDMatrix,f:(LogDouble,LogDouble) => LogDouble) = {
    requirement(that)
    val tmp = for(i <- x.indices) yield f(this.x(i),that.x(i))
    new LDMatrix(tmp.toVector,this.nrow,this.ncol)
  }

  protected def requirement(that:LDMatrix) = require(this.ncol == that.ncol && this.nrow == that.ncol)

}

class LDVector protected (val x:Vector[LogDouble]) {
  def length = x.length
  def indices = x.indices
  def *:(dx:DenseMatrix[Double]):LDVector = {
    require(this.length == dx.cols)
    val tmp = for(i <- 0 until dx.rows) yield
      indices.foldLeft(SemiringLogDouble.zero)((n,j) => n + x(j) * dx(i,j))
    LDVector(tmp.toVector)
  }
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

object LDVector {
  def zeros(n: Int) = new LDVector(Vector.fill(n)(SemiringLogDouble.zero))
  def ones(n: Int) = LDVector(Vector.fill(n)(SemiringLogDouble.one))
  def apply(x:Vector[LogDouble]) = new LDVector(x)
  def apply(x:Array[LogDouble]) = LDVector(x.toVector)
  def apply(x:Array[Double]) = LDVector(x.map(doubleExtra(_).toLogDouble))
  def apply(x:DenseVector[Double]) = LDVector(x.toArray.map(doubleExtra(_).toLogDouble))
}