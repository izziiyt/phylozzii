import org.scalatest.FunSuite
import util.FdFilter

class FastaTest extends FunSuite {
  test("sample.aa and sample.nuc") {
    val aa = "src/test/resources/util/sample.aa"
    val nuc = "src/test/resources/util/sample.nuc"
    val species = "src/test/resources/util/sample.species.txt"
    val nh = "src/test/resources/hg19.100way.nh"
    val cdn = "src/test/resources/codon.table.txt"
    val target = "target/result.txt"
    FdFilter.main(Array(nuc, aa, species, nh, cdn, target))
    //23 32 35
    //3 6 9 12 15 24 27 33 36 42 48 60 63 69 78 81 84 99 102 111 114 123
  }
}