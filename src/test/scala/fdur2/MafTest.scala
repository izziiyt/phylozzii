package fdur2

import org.scalatest.FunSuite
import alignment.Base.{A,C,G,T,N}
import alignment.Base
class MafTest extends FunSuite {
  test("readmaf") {
    def f(xs:List[Array[Base]],ys:List[Array[Base]]):Unit = assert((xs,ys).zipped.forall((x,y) => x sameElements y))
    val xs1 = Maf.readMaf("src/test/resources/fdur2/test1.maf", 10)
    val ys1 = List(Array[Base](C, A, G, C, A, C, T, T), Array[Base](C, A, G, G, A, G, T, T), Array[Base](C, T, G, G, T, C, G, G))
    f(xs1.head,ys1)
    val xs2 = Maf.readMaf("src/test/resources/fdur2/test2.maf", 10)
    val ys2 = Array(
      List(Array[Base](C, A, G, C, A, C, T, T,C,A), Array[Base](C, A, G, G, A, G, T, T,C,A), Array[Base](C, T, G, G, T, C, G, G,C,T)),
      List(Array[Base](G,C,A,C,T,T,A,A,T), Array[Base](G,G,A,G,T,T,N,N,T), Array[Base](G,G,T,C,G,G,N,N,G))
    )
    (xs2,ys2).zipped.foreach((as,bs) => f(as,bs))
  }
}
