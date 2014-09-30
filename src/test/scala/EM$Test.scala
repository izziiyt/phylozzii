import org.scalatest.FunSuite
import scala.io.Source
class EM$Test extends FunSuite {
  val x0 = List[Char](1,1,1)
  val x1 = List[Char](2,2,1)
  val x2 = List[Char](2,3,3)
  val x3 = List[Char](2,2,3)
  val x4 = List[Char](0,0,1)
  val x5 = List[Char](3,1,0)
  val x6 = List[Char](3,0,0)
  val x7 = List[Char](3,1,0)
  val x8 = List[Char](2,1,0)
  val x9 = List[Char](3,1,0)
  val x10 = List[Char](1,1,1)
  val x11 = List[Char](3,1,0)
  val x12 = List[Char](2,3,0)
  val x13 = List[Char](3,1,0)
  //val source = Source.fromFile("../resources/sample.nh")
  //val source = Source.fromURL(getClass.getResource("/sample.nh"))
  //val query = source.getLines().reduce(_ + _)
 // source.close()
  val source = "/home/yuto/projects/fdur/src/test/resources/sample.nh"
  test("EM"){
    val alignments = List(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
    EM(100,source,alignments)
  }
}
