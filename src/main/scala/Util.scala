import scala.io.Source

object Util {
  def getAlignments(al:String):List[List[Char]] = {
    val source = Source.fromFile(al)
    val cols = for{
    //l <- source.getLines().take(100)
      l <- source.getLines()
      chrs = l.split(" ")
    } yield chrs.map(_.toInt.toChar).toList
    source.close()
    cols.toList
  }
}