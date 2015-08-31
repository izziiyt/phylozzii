package fdur

import java.io.{FileOutputStream, PrintWriter}

import breeze.linalg.DenseVector
import util._

object Mstep extends EM {

  private var bpupdate = true

  private var branchupdate = true

  def main(args:Array[String]){
    /*
    args(0):a directory which contains files fdur.Count written
    args(1):a file current parameter written
    args(2):a file current structure and branch length of phylogency tree written
    args(3):a directory which contains log information
    args(4):a file consumed time written
    */
    bpupdate = if(args.length >= 6 && args(5).split('=')(1) == "false" ) false else true
    val tmp = if(args(0).endsWith("/")) "" else "/"
    val countFileList = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".ct")).map(args(0) + tmp + _.getName)
    val os = new FileOutputStream(args(4),true)
    util.printExecutionTime(exe(countFileList,args(1),args(2),args(3)),"mstep",os)
    os.close()
  }

  protected def exe(countFiles:Array[String],paramFile:String,nhFile:String,logDir:String){
    val can = countFiles.map(file2Count)
    val count = can.map(_._1).reduce(_+_) / can.map(_._2).sum
    val model = GTR(Parameters.fromFile(paramFile))
    val Ns = count.Ns.reduce(_+_)
    val tree = FdurTree.fromFile(nhFile)
    val Td:DenseVector[Double] = (count.Fd,tree.branches).zipped.map(_*_).reduce(_+_)
    val pt =
      if(bpupdate)
        new PhylogencyTree(FdurTree.fromFile(nhFile),GTR(Parameters(newB(Ns,Td,model),newPi(Ns,Td,count.ns,model))))
      else
        new PhylogencyTree(FdurTree.fromFile(nhFile),GTR(Parameters(newB(Ns,Td,model),model.pi)))
    pt.setBranch(newT(count.Ns,count.Fd,model))
    logger(pt,count.ll,paramFile,nhFile,logDir)
  }

  protected def file2Count(fin:String):(Count,Int) = {
    val lines = scala.io.Source.fromFile(fin).getLines()
    val n = lines.next().toInt
    val c = Count.fromString(lines.reduce(_+_))
    Pair(c,n)
  }

  protected def logger(pt:PhylogencyTree,ll:Double,paramOut:String,nhOut:String,logDir:String){
    val writeP = new PrintWriter(paramOut)
    writeP.println(pt.model.param)
    writeP.close()
    val writeN = new PrintWriter(nhOut)
    writeN.println(pt.root)
    writeN.close()
    innerLogger(ll,logDir+"/ll.log")
    innerLogger(pt.branches.mkString(sep="\t"),logDir+"/tree.log")
    innerLogger(util.toTSV(pt.model.pi),logDir+"/pi.log")
    innerLogger(pt.model.bList.mkString(sep="\t"),logDir+"/b.log")
  }

  protected def innerLogger[T](content:T,fout:String){
    val write = new PrintWriter(new FileOutputStream(fout,true))
    write.println(content)
    write.close()
  }

}

