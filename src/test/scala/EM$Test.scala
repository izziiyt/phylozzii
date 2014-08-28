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
  val source = Source.fromFile("/home/yuto/data/myFdur/sample.nh")
  val query = source.getLines().foldLeft("")(_ + _)
  source.close()

  test("execute"){
    val alignments = List(x0,x1,x2,x3,x4,x5)
    EM.execute(100,query,alignments)
  }
}
