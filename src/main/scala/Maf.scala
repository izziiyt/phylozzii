import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Sequence(species:String,chr:String,sequence:String){
  def length = sequence.length
}

case class MafUnit(score:Double,seqs:List[Sequence])

case class MafUnitGenerator(file:String,sep:String = """\p{javaWhitespace}+"""){
  val s = Source.fromFile(file)
  val lines = s.getLines()
  private var score = toScore(lines.next.split(sep)(1))
  private var f = true

  def hasNext = f
  def next = if(hasNext) gen else null

  protected def gen:MafUnit = {
    val buf = new ListBuffer[Sequence]
    for(line <- lines;if line != ""){
      val p = line.split(sep)
      val names = p(1).split("\\.")
      p(0) match{
        case "s" => buf += Sequence(names(0),names(1),p(6))
        case "a" =>
          val tmpScore = score
          score = toScore(p(1))
          return MafUnit(tmpScore,buf.toList)
      }
    }
    f = false
    s.close()
    MafUnit(score,buf.toList)
  }

  protected def toScore(arg:String) = arg.diff("score=").toDouble
}