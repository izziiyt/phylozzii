import org.scalatest.FunSuite

class DNATest extends FunSuite {
  test("construct"){
    val x = DNA.fromSeq(List(A,A,G,C,N,N,G))
    assert(x.groups(0) == 935104)
    x.foreach(println)
  }
}
