import breeze.linalg.{trace, sum}
import java.io.{FileOutputStream, OutputStream, PrintWriter}

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
    Visualize.paramViz(logDir + "param.log")
    Visualize.branchViz(logDir + "tree.log")
    Visualize.llViz(logDir + "ll.log")
    val tree = Tree.fromFile(treeFile)
    val param = Parameters.fromFile(paramFile)
    regularize(tree,param,new FileOutputStream(rtreeFile))
  }

  def regularize(tree:Tree,param:Parameters,out:OutputStream){
    val gtr = GTR(param)
    val summ = -(for(i <- 0 to 3) yield gtr.pi(i) * gtr.R(i,i)).sum
    val br = tree.branches.map(summ.*)
    tree.setBranch(br)
    val write = new PrintWriter(out)
    write.println(tree)
    write.flush()
  }
}
