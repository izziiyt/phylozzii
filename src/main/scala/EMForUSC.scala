/**
 * Created by yuto on 14/06/14.
 */

/**
import breeze.linalg._
import scalax.chart.api._
import scalax.chart.Chart._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import math._
import java.io.PrintWriter

object EMForUSC {
  def apply(data:Array[(Int,Int)],paramSize:Int,loop:Int = 1000,fileName:String = "/home/yuto/data/tmp.txt",fileName2:String = "/home/yuto/data/tmp.png") {
    var paramSet = initParamSet(paramSize).toArray
    val buf = ListBuffer[Array[Double]](paramSet)
    for(t <- 0 until loop){
      val (a,lambda,u,ut) = diagDecomp(paramSet)
      val Fs = countF(data,a,lambda,u,ut)
      val Ns = countN(data,a,lambda,u,ut)
      paramSet = reset(Fs,Ns).toArray
      buf += paramSet
    }
    val out = new PrintWriter(fileName)
    def f(x:Array[Double]) = {x.foreach(i => out.print(i.toString + " "));out.println()}
    def g(i:Int)  = {buf(i) zip buf(i+1) map (a => math.pow(a._1 - a._2,2))}
    val q = (1 to buf.size) zip ((0 until buf.size - 1) map (i => pow(sum(g(i)),0.5)))
    val d = (1 to buf.size) zip (buf map (_(0)))
    val chart = XYLineChart(q,title="parameter convergence")
    chart.saveAsPNG(fileName2)
    buf.foreach(f)
    out.close()
    paramSet
  }

  private def initParamSet(n:Int) = {
    val rgen = new Random
    for(i <- 0 until n) yield rgen.nextDouble()
  }

  def diagDecomp(paramSet:Array[Double]):(DenseMatrix[Double],DenseVector[Double],DenseMatrix[Double],DenseMatrix[Double]) = {
    val n = paramSet.size + 1
    val A = DenseMatrix.zeros[Double](n,n)
    for(i <- 0 until n - 1; tmp = paramSet(i)){
      A(i,i) -= tmp
      A(i,i+1) = tmp
      A(i+1,i) = tmp
      A(i+1,i+1) -= tmp
    }
    val (eVals,eVecs) = eigSym(A)
    (A,eVals,eVecs,eVecs.t)
  }

  private def countF(data:Array[(Int,Int)],a:DenseMatrix[Double],lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    val n = lambda.size
    def expA(p:(Int,Int)) = math.exp(a(p._1,p._2))
    def Fd(i:Int)(d:Double,pair:(Int,Int)) = d + divExpMatrix(pair,i,i,lambda,u,ut) / expA(pair)
    (0 until n) map (i => data.foldLeft(0.0)(Fd(i)))
  }

  def countN(data:Array[(Int,Int)],a:DenseMatrix[Double],lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    val n = lambda.size
    def expA(p:(Int,Int)) = math.exp(a(p._1,p._2))
    def Nd(i:Int,j:Int)(d:Double,pair:(Int,Int)) = d + divExpMatrix(pair,i,j,lambda,u,ut) / expA(pair)
    def f (i:Int,j:Int) = math.abs(i - j) match {
      case 1 => a(i,j) * data.foldLeft(0.0)(Nd(i,j))
      case _ => 0.0
    }
    (0 until n) map (i => (0 until n) map (j => f(i,j)))
  }

  def divExpMatrix(pair:(Int,Int),to:Int,from:Int,lambda:DenseVector[Double],u:DenseMatrix[Double],ut:DenseMatrix[Double]) = {
    val end = pair._1
    val beg = pair._2
    val n = lambda.size
    def k(x:Double,y:Double) = if(x == y) exp(x) else (exp(x) - exp(y)) / (x - y)
    val tmp = for(x <- 0 until n; y <- 0 until n) yield u(end,x) * ut(x,to) * u(from,y) * ut(y,beg) * k(lambda(x),lambda(y))
    tmp.sum
  }

  def reset(Fs:IndexedSeq[Double],Ns:IndexedSeq[IndexedSeq[Double]])= {
    val n = Fs.size - 1
    (0 until n) map (i => (Ns(i)(i + 1) + Ns(i + 1)(i)) / (Fs(i) + Fs(i + 1)))
  }
}
**/
