import org.scalatest.FunSuite
import alignment.Base._
import io.Source

class CodonTable$Test extends FunSuite {
  test("codon.table"){
    val x = CodonTable.fromFile("src/test/resources/codon.table.txt")
    assert(x.is4Fold(Codon(A,C,T)))
    assert(!x.is4Fold(Codon(A,A,A)))
    assert(x.is4Fold(Codon(G,G,T)))
    assert(!x.is4Fold(Codon(T,A,A)))
    assert(x.transcript(Codon(A,G,T)) == alignment.AminoAcid.S)
  }

  def f(i:Int):Stream[Int] = i #:: f(i+1)

  def g(s:Iterator[String]):Stream[String] = s.next() #:: g(s)

  test("Stream") {
    val s = f(1)
    println(s.head)
    println(s.head)
    val so = Source.fromFile("src/test/resources/brown.nuc")
    val ss = g(so.getLines())
    println(ss.head)
    println(ss.head)
    println(ss.tail.head)
    so.close()
  }


}
