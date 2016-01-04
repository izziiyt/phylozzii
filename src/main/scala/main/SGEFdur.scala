package main

import java.io._
import biformat.Maf._
import fdur._

object SGEFdur extends SuffStatParser{
  def qmstep(dir: File, pf: File, nh: File, cbf: Boolean): Unit = {
    val files = dir.listFiles.filter(_.getName.endsWith(".txt"))
    val suffs = files.map(EMreader)
    val param = Parameters.fromFile(pf)
    val tree = ModelTree.fromFile(nh)
    val (lgl, brnch, newparam) = Optimizer.mstep(suffs,Model(param), tree.branches)
    writeLine(if(cbf) Parameters(newparam.Bvec,param.pi).toString else newparam.toString,pf, false)
    writeLine(newparam.Bvec.toArray.mkString(",") + "," + newparam.pi.toArray.mkString(","), new File("tmp/param.log.txt"), true)
    val newtree = tree.changeBranches(brnch)
    writeLine(newtree.toString,nh, false)
    writeLine(newtree.branches.mkString(","), new File("tmp/tree.log.txt"), true)
    writeLine(lgl.toString, new File("tmp/lgl.log.txt"), true)
  }

  def writeLine(x:String, f:File, b:Boolean): Unit = {
    val y = new PrintWriter(new FileWriter(f,b))
    y.println(x)
    y.close()
  }

  protected def EMreader(x:File): (VD, List[MD], List[VD], Double, Long) = {
    val reader = new FileReader(x)
    parseAll(suffstat,reader).get
  }

  def qestep(maf: File, nh: File, pf: File, out: File): Unit = {
    //println(scala.collection.parallel.availableProcessors)
    //${al} ${nh} ${count} target/time/e/${SGE_TASK_ID}.time
    val cols = readMaf(maf.getName,10000)
    val tree = ModelTree.fromFile(nh)
    val param = Parameters.fromFile(pf)
    val results = cols.map { c => Optimizer.ldestep(tree, c, Model(param)) }
    val summed = results.reduce {
      (m, x) =>
        val ns = m._1 + x._1
        val Ns = (m._2, x._2).zipped.map(_ + _)
        val Fd = (m._3, x._3).zipped.map(_ + _)
        val l = m._4 + x._4
        val i = m._5 + x._5
        (ns, Ns, Fd, l, i)
    }
    EMprinter(summed, out)
  }

  protected def EMprinter(result:(VD,List[MD],List[VD],Double,Long),out:File): Unit = {
    val w = new BufferedWriter(new FileWriter(out))
    w.write("ns: " + result._1.toArray.mkString("(", ",", ")"))
    w.newLine()
    w.write("Fd: " + result._2.map(x => x.toArray.mkString("(", ",", ")")).mkString(","))
    w.newLine()
    w.write("Ns: " + result._3.map(x => x.toArray.mkString("(",",",")")).mkString(","))
    w.newLine()
    w.write("lgl: " + result._4)
    w.newLine()
    w.write("length: " + result._5)
    w.close()
  }

  protected def GDprinter(result:(VD, MD, List[Double], Double),w:PrintWriter): Unit = {
    w.println("pi: " + result._1.toArray.mkString("(", "\t", ")"))
    w.println("b: " + result._2.toArray.mkString("(", "\t", ")"))
    w.println("branch: " + result._3.mkString("(", "\t", ")"))
    w.println("lgl: " + result._4)
  }
}
