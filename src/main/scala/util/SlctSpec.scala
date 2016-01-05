/*package util

import java.io.{File, PrintWriter}

import scala.io.Source

object SlctSpec {
  def main(args:Array[String]){
    val fs = util.lsf(args(0))
    val is = Source.fromFile(args(2)).getLines().map(_.toInt).toSeq
    dist(fs,args(1),is)
    //exe(fs,args(1),is)
    //count(args(1),is)
  }

  private def exe(ifs:Seq[File],oDirName:String,potentList:Seq[Int]){
    val tmap = (0 to potentList.length).map{x => x -> new PrintWriter(fName(oDirName,x))}.toMap
    val set = potentList.toSet
    var tid:Long = 0
    for(f <- ifs){
      val s = Source.fromFile(f)
      for(l <- s.getLines()){
        val line = l.split(" ").map(_.toInt)
        val c = line.indices.count(x => line(x) < 4 && (set contains x))
        if(tmap contains c){tmap(c).println(tid)}
        tid += 1
      }
      s.close()
    }
    tmap.values.foreach(_.close())
  }

  private def dist(ifs:Seq[File],odir:String,potentList:Seq[Int]){
    val amap = new Array[Long] (101)
    val cmap = new Array[Long] (101)
    val set = potentList.toSet
    for(f <- ifs){
      val s = Source.fromFile(f)
      for(l <- s.getLines();line = l.split(" ").map(_.toInt)){
        val all = line.indices.count(x => line(x) < 4)
        amap(all) += 1
        if(line.indices.count(x => line(x) < 4 && (set contains x)) > 0){cmap(all) += 1}
      }
      s.close()
    }
    val wra = new PrintWriter(fName(odir,100,".dist.txt"))
    val wrc = new PrintWriter(fName(odir,16,".dist.txt"))
    ((0 to 100) zip amap).foreach{case (x,y) => wra.println(x + "," + y)}
    ((0 to 100) zip cmap).foreach{case (x,y) => wrc.println(x + "," + y)}
    wra.close()
    wrc.close()
  }

  private def fName(dir:String,i:Int,suffix:String = ".tid.txt"):String =
    if(dir.endsWith("/")) dir + i + suffix else dir + "/" + i + suffix

  private def count(dir:String,potentList:Seq[Int]){
    val range = 0 to potentList.length
    val tmap = range.map{x => x -> Source.fromFile(fName(dir,x)).getLines().size}.toMap
    val wr = new PrintWriter(fName(dir,0,".num.txt"))
    tmap.foreach(wr.println)
    wr.close()
  }
}
*/