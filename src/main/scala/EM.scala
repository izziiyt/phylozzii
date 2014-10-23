import breeze.linalg.{DenseMatrix, DenseVector,sum,trace}
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math._
import java.io.PrintWriter

object EM{
  def apply(loop:Int,nhFile:String,alignments:List[List[Char]]){
    var pt = new PhylogencyTree(nhFile,GTR())
    val an:Double = alignments.length
    for(i <- 1 to loop){
      val counts = alignments.map(eStep(pt,_))
      pt = mStep(pt,counts,an)
    }
  }

  def test(loop:Int,nhFile:String,alignments:List[List[Char]]){
    val paramLog = ArrayBuffer[Parameters]()
    val branchLog = ArrayBuffer[List[Double]]()
    val derivFile = new PrintWriter("target/derivation.txt")
    var pt = new PhylogencyTree(nhFile,GTR())
    val an:Double = alignments.length
    for(i <- 1 to loop){
      val counts = alignments.map(eStep(pt,_))
      pt = mStep(pt,counts,an)
      paramLog += pt.model.param
      branchLog += pt.branches
    }
    println(pt.branches())
    def f(p:PhylogencyTree,col:List[Char]):(Parameters,List[Double]) = {
      val pt = new PhylogencyTree(p,p.model)
      pt.setAlignment(col)
      pt.inside()
      pt.outside()
      val likelihood = pt.likelihood
      pt.root.setPosterior(likelihood,pt.model)
      pt.deriveLL
    }
    val pairs = alignments.map(f(pt,_))
    val param = pairs.map(_._1).reduceLeft(_ + _)
    Visualize.paramViz(paramLog.toList)
    Visualize.branchViz(branchLog.toList)
    derivFile.print(param + " ")
    val brnc = pairs.map(_._2).reduceLeft((_,_).zipped.map(_ + _))
    derivFile.println("branch " + brnc)
    derivFile.close()
  }

  def mStep(pt:PhylogencyTree,counts:List[Count],an:Double):PhylogencyTree = {
    def foldDouble(xs:List[List[Double]]):List[Double] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ / an)
    def foldMatrix(xs:List[List[DenseMatrix[Double]]]):List[DenseMatrix[Double]] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ / an)
    def foldVector(xs:List[List[DenseVector[Double]]]):List[DenseVector[Double]] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ / an)
    def folding(xs:List[DenseVector[Double]]):DenseVector[Double] = xs.reduce(_ + _) / an
    val FdList:List[DenseVector[Double]] = foldVector(counts.map(_.Fd))
    val NsList:List[DenseMatrix[Double]] = foldMatrix(counts.map(_.Ns))
    val TList:List[Double] = foldDouble(counts.map(_.T))
    val nVec:DenseVector[Double] = folding(counts.map(_.ns))
    println("total likelihood: " + counts.map(_.likelihood).sum)
    val TdVec:DenseVector[Double] = (FdList,TList).zipped.map(_ * _).reduce(_ + _)
    val NsMat:DenseMatrix[Double] = NsList.reduce(_ + _)
    val tmp = new PhylogencyTree(pt,GTR(Parameters(newB(NsMat,TdVec,pt.model),newPi(NsMat,TdVec,nVec,pt.model))))
    tmp.setBranch(newT(NsList,FdList,pt.model) ::: List(0.0))
    tmp
  }

  def newPi(Ns:DenseMatrix[Double],Td:DenseVector[Double],n:DenseVector[Double],m:EvolutionModel) = {
    val u = (0 until 4) map (i => n(i) + sum(Ns(::,i)) - Ns(i,i))
    val v = (0 until 4) map (i => (0 until 4).foldLeft(0.0)((x,j) => if(i != j) x + m.B(j,i) * Td(j) else x))
    calcNewParameter(u.toList,v.toList)
  }

  def newB(Ns:DenseMatrix[Double],Td:DenseVector[Double],m:EvolutionModel) = {
    val u = for(i <- 0 until 4;j <- i+1 until 4) yield Ns(i,j) + Ns(j,i)
    val v = for(i <- 0 until 4;j <- i+1 until 4) yield m.pi(j) * Td(i) + m.pi(i) * Td(j)
    calcNewParameter(u.toList,v.toList)
  }

  def newT(Ns:List[DenseMatrix[Double]],Fd:List[DenseVector[Double]],m:EvolutionModel):List[Double] = {
    val Ns0:List[Double] = Ns.map(m => sum(m) - trace(m))
    (Ns0,Fd).zipped.map((ns0,fd) => ns0 / (0 until 4).foldLeft(0.0)((x,i) => x + abs(m.R(i,i)) * fd(i)))
  }

  def calcNewParameter(u:List[Double],v:List[Double]):DenseVector[Double] = {
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

  def eStep(p:PhylogencyTree,column:List[Char]):Count = {
    val pt = new PhylogencyTree(p,p.model)
    pt.root.setAlignment(column)
    pt.inside(pt.root)
    pt.outside(pt.root)
    val likelihood = pt.root.likelihood(pt.model)
    pt.root.setPosterior(likelihood,pt.model)
    pt.root.count(pt.model)
  }
}
