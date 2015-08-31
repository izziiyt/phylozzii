package fdur2

import scala.collection.mutable.ListBuffer
import scala.io.Source
import alignment.Base

object Maf {
  def readMaf(mf:String,per:Int):Array[List[Array[Base]]] = {
    val it = MafUnitIterator.fromMSA(mf)
    val bases = it.foldLeft(MafUnit.Nil){(n,u) => n + u}.seqs
    val tmp = div[Base](bases,per)
    tmp
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
  def hasNext = nextOne.isDefined
  protected var nextOne:Option[MafUnit] = nexti()

  def next():MafUnit =
    if(!hasNext) sys.error("Nothing in next.")
    else{
      val tmp = nextOne.get
      nextOne = nexti()
      tmp
    }

  def nexti():Option[MafUnit] = {
    val buf = new ListBuffer[String]
    for(line <- lines;if line != "" && !line.startsWith("#")){
      val p = line.split(sep)
      p(0) match{
        case "s" => buf += p(6)
        case "a" => if(buf.nonEmpty) return Some(MafUnit(buf.toList))
        case _ => Unit
      }
    }
    s.close()
    if(buf.nonEmpty) Some(MafUnit(buf.toList))
    else None
  }

}

object MafUnitIterator {
  def fromMSA(file:String,sep:String = """\p{javaWhitespace}+""") = new MafUnitIterator(file ,sep)
}