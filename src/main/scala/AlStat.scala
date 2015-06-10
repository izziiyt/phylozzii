import java.io.{File, PrintWriter}
import scala.io.Source
import alignment.Base

object AlStat {
  def main(args:Array[String]){
    val fs = Util.lsf(args(0))
    val w = new PrintWriter(args(1))
    exe(fs,w)
  }
  def exe(flist:Seq[File],wr:PrintWriter){
    val x = Array.ofDim[Long](100,100)
    for(f <- flist;line <- Source.fromFile(f).getLines()){
      val l = line.replace(" ","").map(Base.fromChar)
      val tmp = (0 to 99).filter{x => l(x) != Base.N}.toSet
      tmp.subsets(2).foreach{a => val k = a.toArray.sorted;x(k(0))(k(1)) += 1}
    }
    x.foreach(y => wr.println(y.mkString(",")))
    wr.close()
  }
}

