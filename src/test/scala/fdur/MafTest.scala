package fdur

import java.io._
import java.nio.file.Files
import java.util.zip.GZIPInputStream
import main.Counter
import org.scalatest.FunSuite
import alignment.Base.{A,C,G,T,N}
import alignment.Base
class MafTest extends FunSuite {
  test("readmaf") {
    //println("hoge.huga".split('.').mkString(","))
    def f(xs:List[Array[Base]],ys:List[Array[Base]]):Unit = assert((xs,ys).zipped.forall((x,y) => x sameElements y))
    val xs1 = Maf.readMaf("src/test/resources/fdur/test1.maf", 10)
    val ys1 = List(Array[Base](C, A, G, C, A, C, T, T), Array[Base](C, A, G, G, A, G, T, T), Array[Base](C, T, G, G, T, C, G, G))
    f(xs1.head,ys1)
    val xs2 = Maf.readMaf("src/test/resources/fdur/test2.maf", 10)
    val ys2 = Array(
      List(Array[Base](C, A, G, C, A, C, T, T,C,A), Array[Base](C, A, G, G, A, G, T, T,C,A), Array[Base](C, T, G, G, T, C, G, G,C,T)),
      List(Array[Base](G,C,A,C,T,T,A,A,T), Array[Base](G,G,A,G,T,T,N,N,T), Array[Base](G,G,T,C,G,G,N,N,G))
    )
    (xs2,ys2).zipped.foreach((as,bs) => f(as,bs))
  }

  /*test("readMaf") {
    Counter.main(Array("src/test/resources/fdur/mafGz/test1.maf.gz"))
    /*val files = new File("src/test/resources/fdur/mafGz").listFiles()

    //val files = new File("/home/izzii/work/100wayz/resources/mafGz").listFiles()
    val counts = Array.fill[Long](5)(0)
    for(f <- files) {
      val s = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(f))))
      var line = s.readLine()
      while(line != null){
        println(line)
        if ( line.head == 's') {
          val xs = line.split("\\s+")(6)
          xs.toCharArray.foreach(x => counts(Base.fromChar(x).toInt) += 1)
        }
        line = s.readLine()
      }
    }
    println(counts.mkString(" "))*/
  }*/

  /*test("convert"){
    Maf.convert("/home/yuto/result.txt","target/result.sp.txt",512,100)
  }*/
}
