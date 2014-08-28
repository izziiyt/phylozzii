import breeze.linalg._
import math.abs
/**
 * Created by yuto on 14/08/21.
 */
abstract class EvolutionModel{
  def transProb(i:Int,j:Int,t:Double):Double
}

object GTR extends EvolutionModel{

  private var param:Parameters = new Parameters(1.0,1.0,1.0,1.0,1.0,1.0,Array[Double](0.25,0.25,0.25,0.25))

  def setParam(x:Parameters){param = x}

  def u = param.u
  def ui = param.ui
  def lambda = param.lambda
  def R = param.R
  def pi = param.pi.toArray
  def B(x:Int,y:Int) = param.B(x,y)
  def transProb(i:Int,j:Int,t:Double):Double = {//i to j
    def tmp:DenseMatrix[Double] = u * diag(lambda.map(x => math.exp(x * t)))
    val transMat:DenseMatrix[Double] = tmp * ui
    transMat(i,j)
  }
}

class Parameters(val a:Double,val b:Double,val c:Double,val d:Double,
                 val e:Double,val f:Double,pix:Array[Double]){
  val pi = new DenseVector[Double](pix)
  private def T:DenseMatrix[Double] = diag(pi.map(math.pow(_,0.5)))
  private def Ti:DenseMatrix[Double] = diag(pi.map(math.pow(_,-0.5)))
  def B(i:Int,j:Int):Double = {
    (i+j,abs(i-j)) match {
      case (1,_) => a
      case (2,2) => b
      case (3,3) => c
      case (3,1) => d
      case (4,2) => e
      case (5,1) => f
      case _ => 0.0
    }
  }
  val R:DenseMatrix[Double] =
    DenseMatrix(
      (0.0,a * pi(1),b * pi(2),c * pi(3)),
      (a * pi(0),0.0,d * pi(2),e * pi(3)),
      (b * pi(0),d * pi(1),0.0,f * pi(3)),
      (c * pi(0),e * pi(1),f * pi(2),0.0))
  (0 until 4) map (i => R(i,i) = 0.0 - sum(R(i,::).t))
  private val tmp:DenseMatrix[Double] = T * R.*(Ti)
  for(i <- 0 until 4;j <- 0 until 4){tmp(j,i) = tmp(i,j)}
  println(tmp)
  val (lambda:DenseVector[Double],eVecs:DenseMatrix[Double]) = eigSym(tmp)
  val u:DenseMatrix[Double] = Ti * eVecs
  val ui:DenseMatrix[Double] = eVecs.t * T
}
