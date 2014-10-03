import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite
import scala.io.Source

/**
 * Created by yuto on 14/08/28.
 */
class EM$Test extends FunSuite {
  val x0 = Array[Int](1,1,1)
  val x1 = Array[Int](2,2,1)
  val x2 = Array[Int](2,3,3)
  val x3 = Array[Int](2,2,3)
  val x4 = Array[Int](0,0,1)
  val x5 = Array[Int](3,1,0)
  val x6 = Array[Int](3,0,0)
  val x7 = Array[Int](3,1,0)
  val x8 = Array[Int](2,1,0)
  val x9 = Array[Int](3,1,0)
  val x10 = Array[Int](1,1,1)
  val x11 = Array[Int](3,1,0)
  val x12 = Array[Int](2,3,0)
  val x13 = Array[Int](3,1,0)
  val source = Source.fromFile("/home/yuto/projects/fdur/src/test/resources/sample.nh")
  val query = source.getLines().foldLeft("")(_ + _)
  source.close()

  test("execute"){

    val alignments = List(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
    EM.execute(100,query,alignments)
  }
}
