package fdur

import java.io.{FileOutputStream, PrintWriter}
import util._

object Estep extends EM{

  def main(args:Array[String]){
    /*
    args(0):a file current parameter written
    args(1):a file alignments written
    args(2):a file structure and branch length of phylogency tree written
    args(3):a file fdur.Count and size of it written
    args(4):a file consumed time written.The file must be different in each jobs.
    */
    val os = new FileOutputStream(args(4),true)
    Util.printExecutionTime(exe(args(0),args(1),args(2),args(3)),"estep",os)
    os.close()
  }

  protected def exe(paramFile:String,alFile:String,nhFile:String,fout:String){
    val param = Parameters.fromFile(paramFile)
    val tree = FdurTree.fromFile(nhFile)
    val pt = new PhylogencyTree(tree,GTR(param))
    val al = Util.getAlignments(alFile)
    val counts = al.map(eStep(pt,_))
    val sumCount = counts.reduce(_+_)
    val writer = new PrintWriter(fout)
    writer.println(counts.length)
    writer.println(sumCount)
    writer.close()
  }
}

