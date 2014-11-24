import java.io.PrintWriter

object Estep extends EM{
  def parEtep(paramFile:String,alFile:String,nhFile:String,fout:String){
    val param = Parameters.fromFile(paramFile)
    val tree = Tree(nhFile)
    val pt = new PhylogencyTree(tree,GTR(param))
    val al = Util.getAlignments(alFile)
    val counts = al.map(eStep(pt,_))
    val sumCount = counts.reduce(_+_)
    val writer = new PrintWriter(fout)
    writer.println(sumCount)
    writer.println(counts.length)
    writer.close()
  }
}

