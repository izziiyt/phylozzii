import breeze.linalg.{DenseMatrix, diag, DenseVector}
import org.scalatest.FunSuite

class GTRTest extends FunSuite {

  test("construct"){

    val gtr = GTR(Parameters(DenseVector[Double](1.0,2.0,3.0,4.0,5.0,6.0),
      DenseVector[Double](1.0,4.0,9.0,16.0)))

    val x = DenseMatrix(
      (-70.0,2.0,6.0,12.0),
      (2.0,-117.0,24.0,40.0),
      (6.0,24.0,-114.0,72.0),
      (12.0,40.0,72.0,-77.0))
    assert(gtr.tmp == x)
  }

  test("eigen value decomposition"){

    val gtr = GTR(Parameters(DenseVector[Double](1.0,1.0,1.0,1.0,1.0,1.0),
    DenseVector[Double](1.0,4.0,9.0,16.0)))
    println(gtr.R * gtr.R * gtr.R)
    println("is same with")
    println(gtr.u * diag(gtr.lambda) * diag(gtr.lambda) * diag(gtr.lambda)* gtr.ui)
  }

  test("toDenseMatrix"){
    val x = Array(1,2,3,4)
    println(new DenseMatrix(2,2,x))
  }
}
