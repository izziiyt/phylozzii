package util

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.LogDouble
import breeze.math.LogDouble.doubleExtra
import breeze.numerics.log
import org.scalatest.FunSuite
import LDVector._
class LDTest extends FunSuite {
  test("LDVector"){
    val xs = LDVector(0.0,1.0,2.0)
    val ys = LDVector(5.0,6.0,7.0)
    assert(xs.toDenseVector == DenseVector(0.0,1.0,2.0))
    assert(xs(0).logValue == Double.NegativeInfinity)
    assert(xs(1).logValue == 0.0)
    assert(xs(2).logValue == log(2.0))
    assert(doubleEqual((xs + ys).toDenseVector,DenseVector(5.0,7.0,9.0),1.0E-10))
    assert(doubleEqual((xs :* ys).toDenseVector,DenseVector(0.0,6.0,14.0),1.0E-10))
    assert(doubleEqual(xs.t * ys,20.0,1.0E-10))
  }

  test("LDMatrix"){
    val xs = DenseVector.ones[LogDouble](2)
    val ys = DenseVector(3.0 toLogDouble ,2.0 toLogDouble)
    val zs = DenseVector(5.0,6.0).toLogDouble
    println(xs)
    println(ys)
    println(xs + ys)
    println(ys + zs)
    //implicit def toLogDouble(xs:DenseVector[Double]): DenseVector[LogDouble] = xs.map(_.toLogDouble)
    //implicit def DenseVectorDoubleExtra(xs:DenseVector[Double]) = new {
      //def toLogDouble: DenseVector[LogDouble] = xs.map(_.toLogDouble)
    //}
    //val xs = DenseVector[LogDouble](Array(0.0,1.0,2.0,3.0,4.0,5.0))
  }

}
