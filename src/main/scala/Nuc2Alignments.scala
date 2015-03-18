import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Nuc2Alignments {
  def apply(file:String,outPut:String){
    val s = Source.fromFile(file)
    val lines = s.getLines()
    val buf = new ArrayBuffer[String]
    var tmp = ""
    for(line <- lines){
      line match{
        case x if x.size > 10 => tmp += line.trim
        case _ => if(! tmp.isEmpty){buf += tmp; tmp = ""}
      }
    }
    buf += tmp
    val n = buf(0).size
    val out = new PrintWriter(outPut)
    for(i <- 0 until n){
      val x = for(b <- buf) yield Maf2Alignments.trans(b(i))
      out.println(x.mkString(" "))
    }
    out.close()
  }
}
