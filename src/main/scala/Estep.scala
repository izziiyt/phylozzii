import java.io.PrintWriter

object Estep {
  def parEtep(paramFile:String,alFile:String,nhFile:String,fout:String){
    val param = Parameters.fromFile(paramFile)
    val tree = Tree(nhFile)
    val pt = new PhylogencyTree(tree,GTR(param))
    val al = Util.getAlignments(alFile)
    val counts = al.map(EM.eStep(pt,_))
    val sumCount = counts.reduce(_+_) / counts.length
    val writer = new PrintWriter(fout)
    writer.println(sumCount)
    writer.close()
  }
}

