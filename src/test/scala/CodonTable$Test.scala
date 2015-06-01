import org.scalatest.FunSuite

class CodonTable$Test extends FunSuite {
  test("codon.table"){
    val x = CodonTable.fromFile("src/test/resources/codon.table.txt")
    assert(x.is4Fold(Codon(A,C,T)))
    assert(!x.is4Fold(Codon(A,A,A)))
    assert(x.is4Fold(Codon(G,G,T)))
    assert(!x.is4Fold(Codon(T,A,A)))
    assert(x.transcript(Codon(A,G,T)) == Ser)
  }
}
