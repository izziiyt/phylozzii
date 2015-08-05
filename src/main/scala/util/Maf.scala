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
  private var score = toScore(lines.next().split(sep)(1))
  private var f = true
  def hasNext = f

  def next():MafUnit = {
    val buf = new ArrayBuffer[Sequence]
    for(line <- lines;if line != ""){
      val p = line.split(sep)
      val names = p(1).split("\\.")
      p(0) match{
        case "s" => buf += Sequence(names(0),names(1),p(6))
        case "a" =>
          val tmpScore = score
          score = toScore(p(1))
          return MafUnit(tmpScore,buf.toArray)
        case _ => Unit
      }
    }
    f = false
    s.close()
    MafUnit(score,buf.toArray)
  }

  protected def toScore(arg:String) = arg.diff("score=").toDouble
}

object MafUnitIterator {
  def fromMSA(file:String,sep:String = """\p{javaWhitespace}+""") = MafUnitIterator(file ,sep)
}