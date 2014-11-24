import breeze.linalg.DenseVector
import org.scalatest.FunSuite

class ParametersTest extends FunSuite {
  test("toString"){
    val param1 = Parameters(DenseVector(0.2,0.3,0.2,0.2,0.1,0.3),DenseVector(0.3,0.1,0.5,0.7))
    val param2 = Parameters.fromString(param1.toString)
    assert(param1 == param2)
  }
}
