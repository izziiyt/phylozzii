package fdur

import java.io.{FileOutputStream, OutputStream, PrintWriter}
import alignment.Base
import breeze.linalg.{DenseMatrix, DenseVector, sum, trace}
import util.PostProc

import scala.annotation.tailrec
import scala.math.pow
import util._

class EM{

  def test(loop:Int,nhFile:String,alignments:Array[Array[Base]],out:OutputStream=System.out){
    var pt = new PhylogencyTree(nhFile,GTR())
    var tmpll = Double.NegativeInfinity
    var diff = false
    var rec = 0
    val os = new FileOutputStream("target/log/tm.log", true)
    do{
      println("-----------------------------------------------------------------------")
      val counts = util.printExecutionTime(alignments.map{x => eStep(pt,x)},"estep",os)
      val tmp = util.printExecutionTime(mStep(pt,counts),"mstep",os)
      pt = tmp._1
      diff = tmp._2 - tmpll > 0.0
      tmpll = tmp._2
      logger(pt,tmp._2,"target/log")
      rec += 1
    }while(diff)
    os.close()
    println("recursion: " + rec)
    PostProc.regularize(pt.root,pt.model.param,out)
  }

  private def logger(pt:PhylogencyTree,ll:Double,logDir:String){
    innerLogger(ll,logDir+"/ll.log")
    innerLogger(pt.branches.mkString(sep="\t"),logDir+"/tree.log")
    innerLogger(util.toTSV(pt.model.pi),logDir+"/pi.log")
    innerLogger(pt.model.bList.mkString(sep="\t"),logDir+"/b.log")
  }

  private def innerLogger[T](content:T,fout:String){
    val write = new PrintWriter(new FileOutputStream(fout,true))
    write.println(content)
    write.close()
  }

  protected def mStep(pt:PhylogencyTree,counts:Array[Count]):(PhylogencyTree,Double) = {
    val sumCount = counts.reduce(_+_) / counts.length
    val Ns = sumCount.Ns.reduce(_+_)
    val Td:DenseVector[Double] = (sumCount.Fd,pt.branches).zipped.map(_*_).reduce(_+_)
    val tmp = new PhylogencyTree(pt,GTR(Parameters(newB(Ns,Td,pt.model),newPi(Ns,Td,sumCount.ns,pt.model))))
    tmp.setBranch(newT(sumCount.Ns,sumCount.Fd,pt.model))
    (tmp,sumCount.ll)
  }

  protected def newPi(Ns:DenseMatrix[Double],Td:DenseVector[Double],n:DenseVector[Double],m:EvolutionModel) = {
    val u = (0 to 3) map (i => n(i) + sum(Ns(::,i)) - Ns(i,i))
    val v = (0 to 3) map (i => (0 to 3).foldLeft(0.0)((x,j) => if(i != j) x + m.B(j,i) * Td(j) else x))
    calcNewParameter(u.toList,v.toList)
  }

  protected def newB(Ns:DenseMatrix[Double],Td:DenseVector[Double],m:EvolutionModel) = {
    val u = for(i <- 0 to 2;j <- i+1 to 3) yield Ns(i,j) + Ns(j,i)
    val v = for(i <- 0 to 2;j <- i+1 to 3) yield m.pi(j) * Td(i) + m.pi(i) * Td(j)
    calcNewParameter(u.toList,v.toList)
  }

  protected def newT(Ns:List[DenseMatrix[Double]],Fd:List[DenseVector[Double]],m:EvolutionModel):List[Double] = {
    val Ns0:List[Double] = Ns.map(x => sum(x) - trace(x))
    (Ns0,Fd).zipped.map((ns0,fd) => - ns0 / (0 to 3).foldLeft(0.0)((x,i) => x + m.R(i,i) * fd(i)))
  }

  protected def calcNewParameter(u:List[Double],v:List[Double]):DenseVector[Double] = {
    if(u.exists(_ < 0) || u.sum <= 0) throw new Exception
    val lmd:Double = (u,v).zipped.collect{case (i,j) if i >= 0 => i - j}.max
    val nlmd = newtonRaphson(lmd,u,v)
    DenseVector((u,v).zipped.map((i,j) => i / (j + nlmd)).toArray)
  }

  @tailrec
  private def newtonRaphson(l:Double,u:List[Double],v:List[Double]):Double = {
    val boy = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / (j + l)} - 1.0
    val mom = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / pow(j + l,2.0)}
    val newL = l + boy / mom
    if(newL.isNaN) sys.error("overfitting error")
    else if(util.doubleChecker(l,newL)) newL
    else newtonRaphson(newL,u,v)
  }

  final def eStep(pt:PhylogencyTree,column:Array[Base]):Count = {
    pt.setColumn(column)
    pt.inside()
    pt.outside()
    pt.setPosterior()
    pt.count
  }
}
