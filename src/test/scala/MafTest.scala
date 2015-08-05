import org.scalatest.FunSuite
import util.Nuc2Al

class MafTest extends FunSuite {
  /*test("parseMaf"){
    val mf = FilteredMafParser("src/test/resources/sample.maf")
  }*/

 /* test("maf to alignments"){
    Maf2Alignments.exe("src/test/resources/sample.maf","src/test/resources/hg19.100way.nh","/home/yuto/resource/hoge",10000)
  }*/

  test("nuc to alignments"){
    Nuc2Al("src/test/resources/brown.nuc","src/test/resources/brown.al")
  }
  test("Set"){
    assert(Array(1,2,3,3,2,1,1,4).toSet.size == 4)
  }
}
