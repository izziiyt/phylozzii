package util

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import fdur.{FdurTree, Tree}

object Maf2Alignments{
  def main(args:Array[String]){
    exe(args(0),args(1),args(2),args(3).toInt)
  }

  protected def exe(maf:String,nh:String,outFilePrefix:String,perSize:Int){
    val gen = MafUnitGenerator(maf)
    val names = FdurTree.fromFile(nh).names
    val map = (names,names.indices).zipped.map(_ -> _).toMap
    val buf = new ArrayBuffer[Array[Int]]
    val printer = Printer(outFilePrefix)
    while(gen.hasNext){
      val unit = gen.next
      for(i <- 0 until unit.seqs.head.length){
        val col = Array.fill(names.length)(4)
        for(seq <- unit.seqs){col(map(seq.species)) = trans(seq.sequence(i))}
        if(isGoodColumn(col)) buf += col
        if(buf.size == perSize){printer(buf);buf.clear()}
      }
    }
  }

  protected def isGoodColumn(col:Array[Int]) = {
    val c = col.foldLeft(0)((x,y) => if(y==4) x+1 else x)
    c+1 < col.length
  }

  def trans(x:Char):Int = {
    x match {
      case 'a' | 'A' => 2
      case 'c' | 'C' => 1
      case 'g' | 'G' => 3
      case 't' | 'T' => 0
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
