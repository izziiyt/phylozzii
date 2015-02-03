import breeze.linalg.{DenseMatrix, DenseVector,sum,trace}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import math.{log,pow,exp}

class EM{

  def test(loop:Int,nhFile:String,alFile:String){
    val alignments = Util.getAlignments(alFile)
    val paramLog = ArrayBuffer[Parameters]()
    val branchLog = ArrayBuffer[List[Double]]()
    val llLog = ArrayBuffer[Double]()
    var pt = new PhylogencyTree(nhFile,GTR())
    for(i <- 1 to loop){
      println(i)
      val counts = alignments.map(eStep(pt,_))
      pt = mStep(pt,counts)
      paramLog += pt.model.param
      branchLog += pt.branches
      llLog += log(pt.likelihood)
    }
    PostProc.regularize(pt.root,pt.model.param,System.out)
    Visualize.paramViz(paramLog.toList)
    Visualize.branchViz(branchLog.toList)
    Visualize.llViz(llLog.toList)
  }

  protected def mStep(pt:PhylogencyTree,counts:Array[Count]):PhylogencyTree = {
    val sumCount = counts.reduce(_+_) / counts.length
    val Ns = sumCount.Ns.reduce(_+_)
    val Td:DenseVector[Double] = (sumCount.Fd,sumCount.T).zipped.map(_*_).reduce(_+_)
    val tmp = new PhylogencyTree(pt,GTR(Parameters(newB(Ns,Td,pt.model),newPi(Ns,Td,sumCount.ns,pt.model))))
    tmp.setBranch(newT(sumCount.Ns,sumCount.Fd,pt.model))
    tmp
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
    @tailrec
    def f(l:Double,loop:Int=0):Double = {
      if(loop != 0 && loop % 100 == 0){println(l);println(loop)}
      val boy = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / (j + l)} - 1.0
      val mom = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / pow(j + l,2.0)}
      val newL = l + boy / mom
      if(Util.doubleChecker(l,newL)) newL else f(newL,loop+1)
    }
    val lmd:Double = (u,v).zipped.collect{case (i,j) if i >= 0 => i - j}.max
    val nlmd = f(lmd)
    DenseVector((u,v).zipped.map((i,j) => i / (j + nlmd)).toArray)
  }

  def eStep(pt:PhylogencyTree,column:Array[Char]):Count = {
    pt.setColumn(column)
    pt.inside()
    pt.outside()
    pt.setPosterior()
    pt.count
  }
}
