import breeze.linalg.{DenseMatrix, DenseVector,sum,trace}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math._

class EM{

  def test(loop:Int,nhFile:String,alignments:List[List[Char]]){
    val paramLog = ArrayBuffer[Parameters]()
    val branchLog = ArrayBuffer[List[Double]]()
    val llLog = ArrayBuffer[Double]()
    var pt = new PhylogencyTree(nhFile,GTR())
    for(i <- 1 to loop){
      val counts = alignments.map(eStep(pt,_))
      pt = mStep(pt,counts)
      paramLog += pt.model.param
      branchLog += pt.branches
    }
    Visualize.paramViz(paramLog.toList)
    Visualize.branchViz(branchLog.toList)
  }

  protected def mStep(pt:PhylogencyTree,counts:List[Count]):PhylogencyTree = {
    val sumCount = counts.reduce(_+_) / counts.length
    println("likelihood : " + sumCount.ll)
    val Ns = sumCount.Ns.reduce(_+_)
    val Td:DenseVector[Double] = (sumCount.Fd,sumCount.T).zipped.map(_*_).reduce(_+_)
    val tmp = new PhylogencyTree(pt,GTR(Parameters(newB(Ns,Td,pt.model),newPi(Ns,Td,sumCount.ns,pt.model))))
    tmp.setBranch(newT(sumCount.Ns,sumCount.Fd,pt.model))
    tmp
  }

  /*protected def qFunction(c:Count,m:EvolutionModel):Double = {
    val n = for(i <- 0 to 3;j <- 0 to 3;if i != j) yield (c.Ns,c.T).zipped.map((x,y) => x(i,j) * log(y * m.R(i,j))).sum
    val f = for(i <- 0 to 3) yield (c.Fd,c.T).zipped.map((x,y) => x(i) * y * m.R(i,i)).sum
    val t = for(i <- 0 to 3) yield c.ns(i) * log(m.pi(i))
    n.sum + f.sum + t.sum
  }*/

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
    @tailrec
    def f(l:Double):Double = {
      val boy = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / (j + l)} - 1
      val mom = (u,v).zipped.foldLeft(0.0){case (x,(i,j)) => x + i / pow(j + l,2.0)}
      val newL = l + boy / mom
      if(isConverge(l,newL)) newL
      else f(newL)
    }

    def isConverge(x:Double,y:Double) = {if(abs(x -y) < exp(-10)) true else false}

    val lmd = (u,v).zipped.collect{case (i,j) if i > 0 => i - j}.max
    val nlmd = f(lmd)
    DenseVector((u,v).zipped.map((i,j) => i / (j + nlmd)).toArray)
  }

  def eStep(pt:PhylogencyTree,column:List[Char]):Count = {
    pt.setColumn(column)
    pt.inside()
    pt.outside()
    pt.setPosterior()
    pt.count
  }
}
