import java.io.PrintWriter

import alignment.{AminoAcid, Base}
import org.scalatest.FunSuite
import org.scalatest.PrivateMethodTester._
import util.{ExonFastaReader, FdFilter}

//ExonFastaReaderTest
class EFRTest extends FunSuite {
  println("hoge")
  val cdntbl = alignment.CodonTable.fromFile("src/test/resources/codon.table.txt")
  println("hoge")

  test("mkCandidate2"){
    val nuc1 = "ATGAGTGAGAGCATCAACTTCTCTCACAACCTAGGCCA".map(Base.fromChar).toArray
    val prt1 = "MSESINFSHNLGQ".map(AminoAcid.fromChar).toArray
    val nuc2 = "GCTCCTGTCTCCCCCCAGGTGTGTGGTGATGCCAGGCATGCCCTTCCCCAGCATCAGGTCTCCAGAGCTGCAGAAGACGACGGCCGACTTGGATCACACTCTTGTGAGTGTCCCCAGTGTTGCAGAG".map(Base.fromChar).toArray
    val prt2 = "LLSPPRCVVMPGMPFPSIRSPELQKTTADLDHTLVSVPSVAE".map(AminoAcid.fromChar).toArray
    val c = new ExonFastaReader(cdntbl)
    val method = PrivateMethod[Array[Int]]('mkCandidates)
    val o = new PrintWriter(System.out)
    val result1 = c invokePrivate method(nuc1,prt1,o)
    assert((Array(23,32,35) zip result1).forall{case (x,y) => x == y})
    val result2 = c invokePrivate method(nuc2,prt2,o)
    assert((Array(3,6,9,12,15,24,27,33,36,42,48,60,63,69,78,81,84,99,102,105,111,114,120,123) zip result2).
      forall{case (x,y) => x == y})
  }
  test("filtered"){
    val nucInf = "src/test/resources/sample.nuc"
    val aaInf = "src/test/resources/sample.aa"
    val indices = FdFilter.file2Indices("src/test/resources/sample.species.txt","src/test/resources/hg19.100way.nh")
    println(indices)
    val reader = new ExonFastaReader(cdntbl)
    reader.filtered(nucInf,aaInf,"target/filtered.al",indices)
  }
  /*test("practical"){
    val nuc = "atg"
    val prt = "M"
    val c = new ExonFastaReaderTest()
    val method = PrivateMethod[Array[Int]]('mkCandidates)
    val result1 = c invokePrivate method(nuc,prt)
    assert((Array() zip result1).forall{case (x,y) => x == y})
  }*/
}
