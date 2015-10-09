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
    if(util.doubleEqual(brnch,tree.branches,1.0E-7) &&
      util.doubleEqual(param.pi,newparam.pi,1.0E-5) &&
      util.doubleEqual(param.Bvec,newparam.Bvec,1.0E-5)
    ) new File("tmp/isConverged").createNewFile()
    if(args.contains("fix_bg"))
      writeLine(Parameters(newparam.Bvec,param.pi).toString,args(2),false)
    else
      writeLine(newparam.toString,args(2),false)
    writeLine(newparam.toString,"tmp/param.log.txt",true)
    writeLine(tree.changeBranches(brnch).toString,args(3),false)
    writeLine(tree.changeBranches(brnch).toString,"tmp/tree.log.txt",true)
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

