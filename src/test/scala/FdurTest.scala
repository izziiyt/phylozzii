
import fdur.EM
import org.scalatest.FunSuite

class FdurTest extends FunSuite{
  val fdurHome = "/home/yuto/usr/local/packages/fdur-0_0_2/"
  val resHome = "src/test/resources/"
  test("compare with fdur"){
    util.Maf2Al.main(Array(fdurHome + "example/huga.maf",resHome + "fdur/test.nh",resHome + "fdur/al","10000"))
    val al = util.getAlignments(resHome + "fdur/al/0.al")
    val em = new EM()
    em.test(100,resHome + "fdur/test.nh",al)
  }
}
