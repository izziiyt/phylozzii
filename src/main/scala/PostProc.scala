

import breeze.linalg.{trace, sum}
import java.io.PrintWriter

object PostProc {

  def main(args:Array[String]){
    exe(args(0),args(1),args(2),args(3),args(4))
  }

  private def exe(paramLog:String,treeLog:String,llLog:String,treeFile:String,paramFile:String){
    Visualize.paramViz(paramLog)
    Visualize.branchViz(treeLog)
    Visualize.llViz(llLog)
    regulalize(treeFile,paramFile)
  }

  private def regulalize(treeFile:String,paramFile:String){
    val tree = Tree.fromFile(treeFile)
    val param = Parameters.fromFile(paramFile)
    val mat = GTR(param).R
    val summ = sum(mat) - trace(mat)
    val br = tree.branches.map(_/summ)
    tree.setBranch(br)
    val write = new PrintWriter(treeFile)
    write.println(tree)
    write.close()
  }
}
