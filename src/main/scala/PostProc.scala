import breeze.linalg.{trace, sum}
import java.io.{FileOutputStream, OutputStream, PrintWriter}
import scala.collection.mutable.ArrayBuffer

object PostProc {

  def main(args:Array[String]){
    /*
    args(0):an input directory which contains log files
    args(1):an input file current structure and branch length of phylogency tree written
    args(2):an input file current parameters written
    args(3):an output file regularized branch lenght of phylogency tree will be writtten
    */
    val tmp = if(args(0).endsWith("/")) "" else "/"
    exe(args(0)+tmp,args(1),args(2),args(3))
  }

  def exe(logDir:String,treeFile:String,paramFile:String,rtreeFile:String){
    val tree = Tree.fromFile(treeFile)
    val param = Parameters.fromFile(paramFile)
    regularize(tree,param,new FileOutputStream(rtreeFile))
  }

  def regularize(tree:Tree,param:Parameters,out:OutputStream){
    val gtr = GTR(param)
    val summ = - (0 to 3).foldLeft(0.0){(x,i) => x + gtr.pi(i) * gtr.R(i,i)}
    val br = tree.branches.map(summ.*)
    tree.setBranch(br)
    val write = new PrintWriter(out)
    write.println(tree)
    write.println(gtr.param)
    write.println(gtr.R :/ summ)
    write.flush()
  }

}
