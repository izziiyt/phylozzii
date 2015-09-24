package main

import java.io.{FileReader, File, FileWriter, PrintWriter}

import fdur._
import main.QReducer._

object QReducer extends SuffStatParser{
  def main(args:Array[String]): Unit = {
    val files = new java.io.File(args(1)).listFiles.filter(_.getName.endsWith(".txt"))
    val suffs = files.map(EMreader).toParArray
    val param = Parameters.fromFile(args(2))
    val tree = ModelTree.fromFile(args(3))
    val (lgl, brnch, newparam) = Optimizer.mstep(suffs,Model(param), tree.branches)
    writeLine(newparam.toString,args(2),false)
    writeLine(newparam.toString,"tmp/param.log.txt",true)
    writeLine(tree.changeBranches(brnch).toString,args(3),false)
    writeLine(tree.changeBranches(brnch).toString,"tmp/tree.log.txt",true)
    writeLine(lgl.toString,args(4),false)
    writeLine(lgl.toString, "tmp/lgl.log.txt", true)
  }

  def writeLine(x:String,f:String,b:Boolean): Unit = {
    val y = new PrintWriter(new FileWriter(f,b))
    y.println(x)
    y.close()
  }

  protected def EMreader(x:File): (VD, List[MD], List[VD], Double, Long) = {
    val reader = new FileReader(x)
    parseAll(suffstat,reader).get
  }
}

