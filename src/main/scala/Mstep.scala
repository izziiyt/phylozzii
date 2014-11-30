import breeze.linalg.DenseVector
import java.io.PrintWriter

object Mstep extends EM{

  def main(args:Array[String]){
    val cfl = new java.io.File(args(0)).listFiles.filter(_.getName.endsWith(".al")).map(_.getName)
    exe(cfl,args(1),args(2),args(3),args(4))
  }

  private def exe(countFiles:Array[String],paramFile:String,nhFile:String,outParamFile:String,outNhFile:String){
    val can = countFiles.map(readCount)
    val count = can.map(_._1).reduce(_+_) / can.map(_._2).sum
    println("likelihood : " + count.ll)
    val model = GTR(Parameters.fromFile(paramFile))
    val Ns = count.Ns.reduce(_+_)
    val Td:DenseVector[Double] = (count.Fd,count.T).zipped.map(_*_).reduce(_+_)
    val pt = new PhylogencyTree(Tree(nhFile),GTR(Parameters(newB(Ns,Td,model),newPi(Ns,Td,count.ns,model))))
    pt.setBranch(newT(count.Ns,count.Fd,model))
    log(pt,outParamFile,outNhFile)
  }

  private def readCount(fin:String):(Count,Int) = {
    val lines = scala.io.Source.fromFile(fin).getLines()
    val c = Count.fromString(lines.next())
    val n = lines.next().toInt
    Pair(c,n)
  }

  private def log(pt:PhylogencyTree,paramout:String,nhout:String){
    val writeP = new PrintWriter(paramout)
    writeP.println(pt.model.param)
    writeP.close()
    val writeN = new PrintWriter(nhout)
    writeN.println(pt.root)
    writeN.close()
  }
}

