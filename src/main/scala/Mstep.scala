import breeze.linalg.DenseVector
import java.io.{FileOutputStream, PrintWriter}

object Mstep extends EM{

  def main(args:Array[String]){
    /*
    args(0):a directory which contains files Count written
    args(1):a file current parameter written
    args(2):a file current structure and branch length of phylogency tree written
    args(3):a file Count and size of the Count written
    args(4):a file consumed time written
    */
    val tmp = if(args(0).endsWith("/")) "" else "/"
    val countFileList = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".ct")).map(args(0) + tmp + _.getName)
    val os = new FileOutputStream(args(4),true)
    Util.printExecutionTime(exe(countFileList,args(1),args(2),args(3)),"mstep",os)
    os.close()
  }

  def exe(countFiles:Array[String],paramFile:String,nhFile:String,logDir:String){
    val can = countFiles.map(file2Count)
    val count = can.map(_._1).reduce(_+_) / can.map(_._2).sum
    val model = GTR(Parameters.fromFile(paramFile))
    val Ns = count.Ns.reduce(_+_)
    val tree = Tree.fromFile(nhFile)
    val Td:DenseVector[Double] = (count.Fd,tree.branches).zipped.map(_*_).reduce(_+_)
    val pt = new PhylogencyTree(Tree.fromFile(nhFile),GTR(Parameters(newB(Ns,Td,model),newPi(Ns,Td,count.ns,model))))
    pt.setBranch(newT(count.Ns,count.Fd,model))
    logger(pt,count.ll,paramFile,nhFile,logDir)
  }

  private def file2Count(fin:String):(Count,Int) = {
    val lines = scala.io.Source.fromFile(fin).getLines()
    val n = lines.next().toInt
    val c = Count.fromString(lines.reduce(_+_))
    Pair(c,n)
  }

  private def logger(pt:PhylogencyTree,ll:Double,paramOut:String,nhOut:String,logDir:String){
    val writeP = new PrintWriter(paramOut)
    writeP.println(pt.model.param)
    writeP.close()
    val writeN = new PrintWriter(nhOut)
    writeN.println(pt.root)
    writeN.close()
    innerLogger(ll,logDir+"/ll.log")
    innerLogger(pt.branches.mkString(sep="\t"),logDir+"/tree.log")
    innerLogger(Util.toTSV(pt.model.pi),logDir+"/pi.log")
    innerLogger(pt.model.bList.mkString(sep="\t"),logDir+"/b.log")
  }

  private def innerLogger[T](content:T,fout:String){
    val write = new PrintWriter(new FileOutputStream(fout,true))
    write.println(content)
    write.close()
  }

}

