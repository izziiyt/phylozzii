import breeze.linalg.{DenseVector, DenseMatrix, diag}
import org.scalatest.FunSuite
import math.exp

/**
 * Created by yuto on 14/08/23.
 */
class ParametersTest extends FunSuite {
  test("constructor"){
    val param = new Parameters(1.0,1.0,1.0,1.0,1.0,1.0,Array.fill(4)(0.25))
    println("pi: " + param.pi)
    println("lambda: " + param.lambda)
    println("R : " + param.R)
    println("R? : " + param.u * diag(param.lambda).*(param.ui))
    println("ui : " + param.ui)
    println("u : " + param.u)
    println(param.u * param.ui)
    val x = DenseMatrix((5,6),(1,3))
    val y = DenseMatrix((2,0),(3,1))
    val z = DenseMatrix((2,2),(1,0))
    assert(x * y.*(z) == DenseMatrix((62,56),(25,22)))
    for(i <- 0 until 4){for(j <- 0 until 4){" " + print(GTR.transProb(i,j,1.0))};println()}
  }
}
