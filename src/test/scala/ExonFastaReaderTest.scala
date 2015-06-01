import org.scalatest.FunSuite
import org.scalatest.PrivateMethodTester._

class ExonFastaReaderTest extends FunSuite {
  val cdntbl = CodonTable.fromFile("src/test/resources/codon.table.txt")
  test("mkCandidates"){
    val nuc1 = "ATGAGTGAGAGCATCAACTTCTCTCACAACCTAGGCCA".map(Base.fromChar).toArray
    val prt1 = "MSESINFSHNLGQ".map(x => AminoAcid.fromString(x.toString)).toArray
    val nuc2 = "GCTCCTGTCTCCCCCCAGGTGTGTGGTGATGCCAGGCATGCCCTTCCCCAGCATCAGGTCTCCAGAGCTGCAGAAGACGACGGCCGACTTGGATCACACTCTTGTGAGTGTCCCCAGTGTTGCAGAG".map(Base.fromChar).toArray
    val prt2 = "LLSPPRCVVMPGMPFPSIRSPELQKTTADLDHTLVSVPSVAE".map(x => AminoAcid.fromString(x.toString)).toArray
    val c = new ExonFastaReader("","",cdntbl)
    val method = PrivateMethod[Array[Int]]('mkCandidates)
    val result1 = c invokePrivate method(nuc1,prt1)
    assert((Array(23,32,35) zip result1).forall{case (x,y) => x == y})
    val result2 = c invokePrivate method(nuc2,prt2)
    assert((Array(3,6,9,12,15,24,27,33,36,42,48,60,63,69,78,81,84,99,102,105,111,114,120,123) zip result2).
      forall{case (x,y) => x == y})
  }
  test("filtered"){
    val nucInf = "src/test/resources/sample.nuc"
    val aaInf = "src/test/resources/sample.aa"
    val reader = new ExonFastaReader(nucInf,aaInf,cdntbl)
    reader.filtered("target/filtered.al")
  }
  test("practical"){
    val nuc = "atg"
    val prt = "M"
    val c = new ExonFastaReaderTest("","",cdntbl)
    val method = PrivateMethod[Array[Int]]('mkCandidates)
    val result1 = c invokePrivate method(nuc,prt)
    assert((Array() zip result1).forall{case (x,y) => x == y})
  }
}
