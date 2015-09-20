package util

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.math.LogDouble
import breeze.math.LogDouble._

class LDMatrix protected (val x: Vector[LogDouble], val nrow: Int, val ncol: Int) {
  require(x.length == nrow * ncol)

  def apply(i: Int, j: Int):LogDouble = {
    require(0 < i && i < nrow && 0 < j && j < ncol)
    x(i + nrow * j)
  }

  def /(v:LogDouble): LDMatrix = LDMatrix(nrow,ncol,x.map(_ / v))

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
      yield that.indices.foldLeft(0.0 toLogDouble)((n,j) => n + that.x(j) * this(i,j))
    LDVector(tmp.toVector)
  }

  protected def requirement(that:LDMatrix) = require(this.ncol == that.ncol && this.nrow == that.ncol)

  def toDenseMatrix: DenseMatrix[Double] = new DenseMatrix[Double](nrow,ncol,x.toArray.map(_.value))

}


object LDMatrix {
  def apply(nrow: Int, ncol: Int,x: Vector[LogDouble]): LDMatrix = new LDMatrix(x,nrow,ncol)
  def apply( nrow: Int, ncol: Int,x: Seq[LogDouble]): LDMatrix = LDMatrix(nrow,ncol,x.toVector)
  def zeros(x: Int, y: Int): LDMatrix = {
    new LDMatrix(Vector.fill(x * y)(0.0 toLogDouble),x,y)
  }
  implicit def DenseMatrixDoubleExtra(x: DenseMatrix[Double]) = new {
    def toLDMatrix: LDMatrix = {
      val tmp = for(j <- 0 until x.cols;i <- 0 until x.rows) yield  x(i,j).toLogDouble
      LDMatrix(x.rows,x.cols,tmp.toVector)
    }
  }
  val x = DenseVector.ones[LogDouble](4)
}
