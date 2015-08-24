package util

import java.io.PrintWriter
import alignment.Base
import scala.collection.mutable.ArrayBuffer
import fdur.FdurTree

object Maf2Al{
  def main(args:Array[String]){
    /*
    args(0): target .maf file
    args(1): newick format file
    args(2): output directory
    args(3): how many lines per one output file
     */
    exe(args(0),args(1),args(2),args(3).toInt)
  }

  protected def exe(maf:String,nh:String,od:String,perSize:Int){
    val mits = MafUnitIterator.fromMSA(maf)
    val names = FdurTree.fromFile(nh).names
    val map = (names,names.indices).zipped.map(_ -> _).toMap
    val buf = new ArrayBuffer[Array[Base]]
    val printer = Printer(od)
    for(unit <- mits){
      for(i <- unit.seqs.head.indices){
        val col = Array.fill[Base](names.length)(Base.N)
        for(seq <- unit.seqs){col(map(seq.species)) = Base.fromChar(seq.sequence(i))}
        //if(isGoodColumn(col)) buf += col
        buf += col
        if(buf.size == perSize){printer(buf);buf.clear()}
      }
    }
    if(buf.nonEmpty) printer(buf)
  }

  protected def isGoodColumn(col:Array[Base]) = {
    val c = col.foldLeft(0)((x,y) => if(y.isN) x+1 else x)
    c+1 < col.length
  }

  sealed case class Printer(outDir:String){
    val slashedOd = if(outDir.endsWith("/")) outDir else outDir + "/"
    private var count = 0
    def apply(buf:ArrayBuffer[Array[Base]]){
      val out = new PrintWriter(slashedOd + count + ".al")
      buf.foreach(b => out.println(b.mkString("")))
      out.close()
      count += 1
    }
  }
}
