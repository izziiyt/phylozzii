package util

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Sequence(species:String,chr:String,sequence:String){
  def length = sequence.length
  def indices = 0 until length
}

case class MafUnit(score:Double,seqs:Array[Sequence])

case class MafUnitIterator private (file:String,sep:String = """\p{javaWhitespace}+""") extends Iterator[MafUnit] {
  val s = Source.fromFile(file)
  val lines = s.getLines()
  private var score = 0.0
  //private var f = true

  protected var nextOne:Option[MafUnit] = nexti()

  def hasNext = if(nextOne.isDefined) true else false

  def next():MafUnit = {
    if(nextOne.isEmpty) sys.error("No next one")
    val tmp = nextOne.get
    nextOne = nexti()
    tmp
  }

  protected def nexti():Option[MafUnit]  = {
    val buf = new ArrayBuffer[Sequence]
    for(line <- lines;if line != "" && !line.startsWith("#")){
      val p = line.split(sep)
      val names = p(1).split("\\.")
      p(0) match{
        case "s" =>
          val chr = if(names.length > 1) names(1) else ""
          buf += Sequence(names(0),chr,p(6))
        case "a" =>
          val tmpScore = score
          score = toScore(p(1))
          if(buf.nonEmpty) return Some(MafUnit(tmpScore,buf.toArray))
        case _ =>
          Unit
      }
    }
    s.close()
    if(buf.nonEmpty) Some(MafUnit(score,buf.toArray))
    else None
  }

  protected def toScore(arg:String) = arg.diff("score=").toDouble
}

object MafUnitIterator {
  def fromMSA(file:String,sep:String = """\p{javaWhitespace}+""") = MafUnitIterator(file ,sep)
}