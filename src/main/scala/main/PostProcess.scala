package main

import java.io.PrintWriter

import fdur._

object PostProcess {
  def main(args: Array[String]): Unit = {
    //${al} ${nh} ${count} target/time/e/${SGE_TASK_ID}.time
    val param = Parameters.fromFile(args(1))
    val tree = ModelTree.fromFile(args(2))
    val (newbrnch, newparam) = Optimizer.regularize(tree.branches, param)
    val x = new PrintWriter(args(2))
    x.println(tree.changeBranches(newbrnch))
    x.close()
    val y = new PrintWriter(args(1))
    y.println(newparam)
    y.close()
  }
}