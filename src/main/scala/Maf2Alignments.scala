import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

object Maf2Alignments{
  def main(args:Array[String]){
    exe(args(0),args(1),args(2),args(3).toInt)
  }

  def exe(maf:String,nh:String,outFilePrefix:String,perSize:Int){
    val mafunits = FilteredMafParser(maf)
    val names = Tree.fromFile(nh).names
    val map = (names,0 until names.length).zipped.map(_ -> _).toMap
    val buf = new ArrayBuffer[Array[Int]]
    val printer = Printer(outFilePrefix)
    for(unit <- mafunits){
      for(i <- 0 until unit.seqs(0).length){
        val col = Array.fill(names.length)(4)
        for(seq <- unit.seqs){col(map(seq.chr.name)) = trans(seq.seq(i))}
        if(isGoodColumn(col)) buf += col
        if(buf.size == perSize){printer(buf);buf.clear()}
      }
    }
  }

  private def isGoodColumn(col:Array[Int]) = {
    val c = col.foldLeft(0)((x,y) => if(y==4) x+1 else x)
    c+1 < col.size
  }

  def trans(x:Char):Int = {
    x match {
      case 'a' | 'A' => 0
      case 'c' | 'C' => 1
      case 'g' | 'G' => 2
      case 't' | 'T' => 3
      case _ => 4
    }
  }

  sealed case class Printer(outPrefix:String){
    private var count = 0
    def apply(buf:ArrayBuffer[Array[Int]]){
      val out = new PrintWriter(outPrefix + "." + count + ".al")
      buf.foreach(b => out.println(b.mkString(" ")))
      out.close()
      count += 1
    }
  }
}
