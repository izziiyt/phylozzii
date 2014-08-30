import breeze.linalg.{DenseMatrix, DenseVector,sum,trace}
import scala.annotation.tailrec
import scala.math._

/**
 * Created by yuto on 14/08/28.
 */
object EM{
  def execute(loop:Int,nhFile:String,alignments:List[Array[Int]]){
    val pt = new PhylogencyTree(nhFile)
    val an:Double = alignments.length
    def foldDouble(xs:Seq[List[Double]]):List[Double] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ * (1.0 / an))
    def foldMatrix(xs:Seq[List[DenseMatrix[Double]]]):List[DenseMatrix[Double]] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ * (1.0 / an))
    def foldVector(xs:Seq[List[DenseVector[Double]]]):List[DenseVector[Double]] = xs.reduce((x,y) => (x,y).zipped.map(_ + _)).map(_ * (1.0 / an))
    def folding(xs:Seq[DenseVector[Double]]):DenseVector[Double] = xs.reduce(_ + _) * (1.0 / an)
    for(i <- 0 until loop){
      val counts = alignments.map(pt.eStep)
      val FdList:List[DenseVector[Double]] = foldVector(counts.map(_._1))
      val NsList:List[DenseMatrix[Double]] = foldMatrix(counts.map(_._2))
      val TList:List[Double] = foldDouble(counts.map(_._3))
      val nVec:DenseVector[Double] = folding(counts.map(_._4))
      println("total likelihood: " + counts.map(_._5).sum)
      val TdVec:DenseVector[Double] = (FdList,TList).zipped.map(_ * _).reduce(_ + _)
      val NsMat:DenseMatrix[Double] = NsList.reduce(_ + _)
      val B:Array[Double] = newB(NsMat,TdVec)
      GTR.setParam(new Parameters(B(0),B(1),B(2),B(3),B(4),B(5),newPi(NsMat,TdVec,nVec)))
      pt.setBranch(newT(NsList,FdList))
    }
  }



  def newPi(Ns:DenseMatrix[Double],Td:DenseVector[Double],n:DenseVector[Double]) = {
    val u = (0 until 4) map (i => n(i) + sum(Ns(::,i)) - Ns(i,i))
    val v = (0 until 4) map (i => (0 until 4).foldLeft(0.0)((x,j) => if(i != j) x + GTR.B(j,i) * Td(j) else x))
    calcNewParameter(u.toList,v.toList)
  }

  def newB(Ns:DenseMatrix[Double],Td:DenseVector[Double]) = {
    val u = for(i <- 0 until 4;j <- i+1 until 4) yield Ns(i,j) + Ns(j,i)
    val v = for(i <- 0 until 4;j <- i+1 until 4) yield GTR.pi(j) * Td(i) + GTR.pi(i) * Td(j)
    calcNewParameter(u.toList,v.toList)
  }

  def newT(Ns:List[DenseMatrix[Double]],Fd:List[DenseVector[Double]]):List[Double] = {
    val Ns0:List[Double] = Ns.map(m => sum(m) - trace(m))
    (Ns0,Fd).zipped.map((ns0,fd) => ns0 / (0 until 4).foldLeft(0.0)((x,i) => x + abs(GTR.R(i,i)) * fd(i)))
  }

  def calcNewParameter(u:List[Double],v:List[Double]):Array[Double] = {
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
    (u,v).zipped.map((i,j) => i / (j + nlmd)).toArray
  }
}
