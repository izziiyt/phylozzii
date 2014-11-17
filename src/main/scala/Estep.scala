import scala.io.Source

object Estep {
/*  def parEtep(paramFile:String,alFile:String,nhFile:String,outFile:String){
    val param = getParameter(paramFile)
    val tree = Tree(nhFile)
    val pt = new PhylogencyTree(tree,GTR(param))
    val al = getAlignments(alFile)
    val counts = al.map(EM.eStep(pt,_))
    val sumCount = counts.reduce(_+_) / counts.length
    writeCount(outFile,sumCount,al.length)
  }

  private def getAlignments(al:String):List[List[Char]] = {
    val source = Source.fromFile(al)
    val cols = for{
    //l <- source.getLines().take(100)
      l <- source.getLines()
      chrs = l.split(" ")
    } yield chrs.map(_.toInt.toChar).toList
    source.close()
    cols.toList
  }*/
}

