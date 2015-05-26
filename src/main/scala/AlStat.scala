import java.io.{File, PrintWriter}
import scala.io.Source

object AlStat {
 /* def main(args:Array[String]){
    val fs = Util.lsf(args(0))
    val is = Source.fromFile(args(2)).getLines().map(_.toInt).toSeq
    val w = new PrintWriter(args(1))
    exe(fs,w,is)
  }
  def exe(flist:Seq[File],wr:PrintWriter,potentList:Seq[Int]){
    val set = potentList.toSet
    val x = Array.ofDim[Long](100,100)
    for(f <- flist;line <- Source.fromFile(f).getLines()){
      val l = line.map(_.toInt)
      val tmp = (0 to 99).filter{x => l(x) < 4 && (set contains l(x))}
        var s = scala.collection.mutable.Set[Int]()
        (0 until tmp.length).foreach{i => if(tmp(i)) s += i}
        s.subsets(2).foreach{a => val k = a.toArray.sorted;x(k(0))(k(1)) += 1}
      }
    }
    x.foreach(y => wr.println(y.mkString("\t")))
    wr.close()
  }*/
}
