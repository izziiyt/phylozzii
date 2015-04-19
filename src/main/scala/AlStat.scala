import java.io.PrintWriter
import scala.io.Source

object AlStat {
  def main(args:Array[String]){
    exe(args(0),args(1))
  }
  def exe(dir:String,result:String){
    val flist = Util.lsf(dir)
    val x = Array.ofDim[Long](100,100)
    for(f <- flist){
      val lines = Source.fromFile(f).getLines()
      for(l <- lines){
        val tmp = l.split(" ").map{x => if(x.toInt > 3) false else true}.toArray
        var s = scala.collection.mutable.Set[Int]()
        (0 until tmp.length).foreach{i => if(tmp(i)) s += i}
        s.subsets(2).foreach{a => val k = a.toArray.sorted;x(k(0))(k(1)) += 1}
      }
    }
    val writer = new PrintWriter(result)
    x.foreach(y => writer.println(y.mkString("\t")))
    writer.close()
  }
}
