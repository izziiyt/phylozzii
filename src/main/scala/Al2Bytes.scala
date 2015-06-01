import java.io._
import scala.io.Source

object Al2Bytes{
  def main(args:Array[String]){
    exe(args(0),args(1))
  }

  def al2Bytes(inf:String,ouf:String){
    val in = Source.fromFile(inf)
    val out = new PrintWriter(new FileOutputStream(ouf,true))
    for(line <- in.getLines()){
      val xs = line.split(" ").map(x => if(x.toInt > 3) 0 else 1)
      out.println(xs.mkString(" "))
    }
    in.close()
    out.close()
  }

  def exe(dir:String,out:String){
    val flist = Util.lsf(dir)
    flist.foreach(println)
    val writer = new PrintWriter(out)
    for(f <- flist){
      val lines = Source.fromFile(f).getLines()
      for(l <- lines){
        val tmp = l.split(" ").map{x => if(x.toInt > 3) false else true}
        var s = scala.collection.mutable.Set[Int]()
        tmp.indices.foreach{i => if(tmp(i)) s += i}
        writer.println(s.mkString(" "))
      }
    }
    writer.close()
  }
}