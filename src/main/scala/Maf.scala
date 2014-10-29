import scala.util.parsing.combinator.JavaTokenParsers
import java.io.FileReader

class FilteredMaf extends JavaTokenParsers {
  def units: Parser[List[MafUnit]] = rep(unit)
  def unit: Parser[MafUnit] =
    aline~rep(sline) ^^ {case scre~lines => MafUnit(scre,lines)}
  def aline: Parser[Double] =
    "a"~"score="~>floatingPointNumber ^^ {case score => score.toDouble}
  def sline: Parser[Sequence] =
    "s"~>chromosome~index~sequence ^^
      {case chromosome~index~sequence => Sequence(S,chromosome,index,sequence)}
  def chromosome: Parser[Chromosome] =
    ident~"."~ident ^^ {case name~"."~chr => Chromosome(name,chr)}
  def index: Parser[Index] =
    floatingPointNumber~floatingPointNumber~"[\\+\\-\\?]".r~floatingPointNumber ^^
      {case index~_~pm~_ => if(pm == "+") Index(index.toInt,0) else Index(index.toInt,1)}
  def sequence: Parser[String] = "[atcgnATCGN-]*".r
}

object FilteredMafParser extends FilteredMaf {
  def apply(file:String):List[MafUnit] = {
    val reader = new FileReader(file)
    val tmp = parseAll(units,reader).get
    reader.close()
    tmp
  }
}

case class Chromosome(name:String,chr:String)
case class Index(index:Int,direction:Char)

case class Sequence(feature:Feature,chr:Chromosome,index:Index,seq:String){
  def length = seq.length
}

case class MafUnit(score:Double,seqs:List[Sequence])

case class MafHeader(text:String)

sealed trait Feature{
  def isS = false
}
case object S extends Feature{
  override def isS = true
}
case object E extends Feature
case object I extends Feature

