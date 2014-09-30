import breeze.linalg.{sum, accumulate, DenseMatrix}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PhylogencyTreeTest extends FunSuite {
  test("breeze.linalg"){
    val a = DenseMatrix((1,2),(3,4))
    assert(sum(a) == 10)
    println(sum(a))
  }
}
