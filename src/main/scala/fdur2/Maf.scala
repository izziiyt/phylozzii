package fdur2

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import alignment.Base

object Maf {
  def readMaf(mf:String,per:Int):Array[List[Array[Base]]] = {
    val it = MafUnitIterator.fromMSA(mf)
    val bases = it.reduceLeft{(n,u) => n + u}.seqs
    val tmp = div(bases,per)
    tmp
  }

  def div(seqs:List[Array[Base]],size:Int):Array[List[Array[Base]]] = {
    @tailrec
    def f(xs:List[Array[Base]],ys:List[List[Array[Base]]],index:Int):Array[List[Array[Base]]] = {
      if (xs.head.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map{x => x.splitAt(index)}.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs,Nil,size)
  }

}

class MafUnit(val seqs:List[Array[Base]]){
  require(seqs.forall(_.length == seqs.head.length))
  def +(that:MafUnit) = new MafUnit((seqs,that.seqs).zipped.map(_ ++ _))
}


object MafUnit {
  def apply(seq:List[String]) = new MafUnit(seq.map(xs => xs.toCharArray.map(x => Base.fromChar(x))))
  def Nil = new MafUnit(List[Array[Base]]())
}

class MafUnitIterator private (file:String,sep:String = """\p{javaWhitespace}+""") extends Iterator[MafUnit] {
  val s = Source.fromFile(file)
  val lines = s.getLines()
  protected var nextOne:Option[MafUnit] = nexti()
  def hasNext = nextOne.isDefined

  def next():MafUnit = {
    if (!hasNext) sys.error("Nothing in next.")
    else {
      val tmp = nextOne.get
      nextOne = nexti()
      tmp
    }
  }

  def nexti():Option[MafUnit] = {
    if(s.isEmpty) return None
    val buf = new ListBuffer[String]
    for(line <- lines;if line != "" && !line.startsWith("#")){
      val p = line.split(sep)
      p(0) match{
        case "s" => buf += p(6)
        case "a" => if(buf.nonEmpty) return Some(MafUnit(buf.toList))
        case _ => Unit
      }
    }
    if(buf.nonEmpty) Some(MafUnit(buf.toList))
    else{
      s.close()
      None
    }
  }

}

object MafUnitIterator {
  def fromMSA(file:String,sep:String = """\p{javaWhitespace}+""") = new MafUnitIterator(file ,sep)
}