import java.io.{File, PrintWriter}
import scala.io.Source

object SlctSpec {
  def main(args:Array[String]){
    val fs = Util.lsf(args(0))
    val wi = new PrintWriter(args(1))
    val wj = new PrintWriter(args(2))
    val is = Source.fromFile(args(3)).getLines().map(_.toInt).toSeq
    exe(fs,wi,wj,is)
    wi.close()
    wj.close()
  }
  def exe(fs:Seq[File],wtid:PrintWriter,wnum:PrintWriter,potentList:Seq[Int]){
    val m = (0 until potentList.length).map(_ -> List[Long]()).toMap
    println(m.size)
    val set = potentList.toSet
    var tid:Long = 0
    for(f <- fs){
      val s = Source.fromFile(f)
      for(l <- s.getLines()){
        val line = l.split(" ").map(_.toInt)
        val c = (0 to 99).count(x => line(x) < 4 && (set contains x))
        if(m contains c){tid :: m(c)}
        tid += 1
      }
      s.close()
    }
    m.foreach{case (x,y) => wtid.println(x + " " + y.mkString(" "))}
    m.foreach{case (x,y) => wnum.println(x + " " + y.length)}
  }
}
