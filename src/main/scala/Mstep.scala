import breeze.linalg.DenseVector
import java.io.{FileOutputStream, PrintWriter}
import math.log

object Mstep extends EM{

  def main(args:Array[String]){
    val tmp = if(args(0).endsWith("/")) "" else "/"
    val cfl = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".ct")).map(args(0) + tmp + _.getName)
    Util.printExecutionTime(exe(cfl,args(1),args(2),args(3),args(4),args(5)),"mstep")
  }

  private def exe(countFiles:Array[String],paramFile:String,nhFile:String,outParamFile:String,outNhFile:String,logDir:String){
    val can = countFiles.map(readCount)
    val count = can.map(_._1).reduce(_+_) / can.map(_._2).sum
    val out = new PrintWriter("errLog")
    out.println(can.map(_._2).sum)
    out.close()
    val model = GTR(Parameters.fromFile(paramFile))
    val Ns = count.Ns.reduce(_+_)
    val Td:DenseVector[Double] = (count.Fd,count.T).zipped.map(_*_).reduce(_+_)
    val pt = new PhylogencyTree(Tree(nhFile),GTR(Parameters(newB(Ns,Td,model),newPi(Ns,Td,count.ns,model))))
    pt.setBranch(newT(count.Ns,count.Fd,model))
    logger(pt,count.ll,outParamFile,outNhFile,logDir)
  }

  private def readCount(fin:String):(Count,Int) = {
    val lines = scala.io.Source.fromFile(fin).getLines()
    val n = lines.next().toInt
    val c = Count.fromString(lines.reduce(_+_))
    Pair(c,n)
  }

  private def logger(pt:PhylogencyTree,ll:Double,paramout:String,nhout:String,logDir:String){
    val writeP = new PrintWriter(paramout)
    writeP.println(pt.model.param)
    writeP.close()
    val writeN = new PrintWriter(nhout)
    writeN.println(pt.root)
    writeN.close()
    innerLogger(ll,logDir+"/log.ll")
    innerLogger(pt.root,logDir+"/log.tree")
    innerLogger(pt.model.param,logDir+"/log.param")
  }

  private def innerLogger[T](content:T,fout:String){
    val write = new PrintWriter(new FileOutputStream(fout,true))
    write.println(">")
    write.println(content)
    write.close()
  }

}

