package util

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.math.LogDouble
import breeze.math.LogDouble.doubleExtra
import breeze.math.LogDouble.SemiringLogDouble
import breeze.numerics.exp

class LDMatrix protected (val x: Vector[LogDouble], val nrow: Int, val ncol: Int) {
  require(x.length == nrow * ncol)

  def apply(i: Int, j: Int):LogDouble = {
    require(0 < i && i < nrow && 0 < j && j < ncol)
    x(i + nrow * j)
  }

  def /(v:LogDouble): LDMatrix = LDMatrix(x.map(_ / v),nrow,ncol)

  def t: LDMatrix = {
    new LDMatrix(x,ncol,nrow) {
      override def apply(i: Int, j: Int):LogDouble = {
        require(0 < i && i < nrow && 0 < j && j < ncol)
        x(i * ncol + j)
      }
    }
  }

  def :*(that:LDMatrix) = sequentialFunc(that,(x: LogDouble, y: LogDouble) => x * y)

  def +(that:LDMatrix) = sequentialFunc(that,(x: LogDouble, y: LogDouble) => x + y)

  protected def sequentialFunc(that:LDMatrix,f:(LogDouble,LogDouble) => LogDouble) = {
    requirement(that)
    val tmp = for(i <- x.indices) yield f(this.x(i),that.x(i))
    new LDMatrix(tmp.toVector,this.nrow,this.ncol)
  }

  def *(that:LDVector): LDVector = {
    require(this.ncol == that.length)
    val tmp = for(i <- 0 until this.nrow)
      yield that.indices.foldLeft(SemiringLogDouble.zero)((n,j) => n + that.x(j) * this(i,j))
    LDVector(tmp.toVector)
  }

  protected def requirement(that:LDMatrix) = require(this.ncol == that.ncol && this.nrow == that.ncol)

  def toDenseMatrix: DenseMatrix[Double] = new DenseMatrix[Double](nrow,ncol,x.toArray.map(_.value))

}


object LDMatrix {
  def apply(x: Vector[LogDouble], nrow: Int, ncol: Int): LDMatrix = new LDMatrix(x,nrow,ncol)
  def apply(x: Seq[LogDouble], nrow: Int, ncol: Int): LDMatrix = LDMatrix(x.toVector,nrow,ncol)
  def apply(x: DenseMatrix[Double]): LDMatrix = {
    val tmp = for(j <- 0 until x.cols;i <- 0 until x.rows) yield doubleExtra(x(i,j)).toLogDouble
    LDMatrix(tmp.toVector,x.rows,x.cols)
  }
  def zeros(x: Int, y: Int): LDMatrix = {
    new LDMatrix(Vector.fill(x * y)(SemiringLogDouble.zero),x,y)
  }
}

class TraversedLDVector(val x:Vector[LogDouble]){
  def length = x.length
  def *(that: LDVector): LogDouble = {
    require(that.length == this.length)
    x.indices.foldLeft(SemiringLogDouble.zero){(n,i) => n + (that.x(i) * x(i))}
  }
}

class LDVector protected (val x:Vector[LogDouble]) {
  def length = x.length
  def indices = x.indices
  def t: TraversedLDVector = new TraversedLDVector(x)
  def apply(i: Int): LogDouble = x(i)
  /*def *:(dx:DenseMatrix[Double]):LDVector = {
    require(this.length == dx.cols)
    val tmp = for(i <- 0 until dx.rows) yield
      indices.foldLeft(SemiringLogDouble.zero)((n,j) => n + x(j) * dx(i,j))
    LDVector(tmp.toVector)
  }*/
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
  def zeros(n: Int): LDVector = new LDVector(Vector.fill(n)(SemiringLogDouble.zero))
  def ones(n: Int): LDVector = LDVector(Vector.fill(n)(SemiringLogDouble.one))
  def apply(x:Vector[LogDouble]): LDVector = new LDVector(x)
  def apply(x:Array[LogDouble]): LDVector = LDVector(x.toVector)
  def apply(x:Array[Double]): LDVector = LDVector(x.map(doubleExtra(_).toLogDouble))
  def apply(x:DenseVector[Double]):LDVector = LDVector(x.toArray.map(doubleExtra(_).toLogDouble))
}