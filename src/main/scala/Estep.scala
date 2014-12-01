import java.io.PrintWriter

object Estep extends EM{

  def main(args:Array[String]){
    Util.printExecutionTime(exe(args(0),args(1),args(2),args(3)),"estep")
  }

  private def exe(paramFile:String,alFile:String,nhFile:String,fout:String){
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

