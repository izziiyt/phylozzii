package util

import org.scalatest.FunSuite

class FastaTest extends FunSuite{
  test("sample.aa and sample.nuc") {
    val aa = "src/test/resources/util/sample.aa"
    val nuc = "src/test/resources/util/sample.nuc"
    val species = "src/test/resources/util/sample.species.txt"
    val nh = "src/test/resources/hg19.100way.nh"
    val cdn = "src/test/resources/codon.table.txt"
    val target = "target/result.txt"
    FdFilter.main(Array(nuc, aa, species, nh, cdn, target))
  }
}
