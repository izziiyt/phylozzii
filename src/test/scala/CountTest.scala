import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite

class CountTest extends FunSuite {
  test("toString"){
    val c1 = Count(List(DenseVector(0.1,0.2),DenseVector(0.3,0.4)),
    List(DenseMatrix((1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0)),
      DenseMatrix((1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0),(1.0,2.0,3.0,4.0))),
    List(2.0,3.0),DenseVector(2.0,2.0,3.0),0.9)
    println(c1)
    val c2 = Count.fromString(c1.toString)
    assert(c1 == c2)
  }
}
