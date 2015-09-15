/*package util

import java.io.{FileOutputStream, OutputStream, PrintWriter}
import fdur.{FdurTree, Parameters, Tree}

object PostProc {

  def main(args:Array[String]){
    /*
    args(0):an input file current structure and branch length of phylogency tree written
    args(1):an input file current parameters written
    args(2):an output file regularized branch lenght of phylogency tree will be writtten
    */
    exe(args(0),args(1),args(2))
  }

  def exe(treeFile:String,paramFile:String,rtreeFile:String){
    val tree = FdurTree.fromFile(treeFile)
    val param = Parameters.fromFile(paramFile)
    regularize(tree,param,new FileOutputStream(rtreeFile))
  }

  def regularize(tree:FdurTree,param:Parameters,out:OutputStream){
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

}*/
