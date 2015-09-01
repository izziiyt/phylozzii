package fdur2

import breeze.linalg._
import breeze.numerics.pow

import scala.annotation.tailrec

sealed trait ModelTrait {
  def R: MD

  def u: MD

  def ui: MD

  def lambda: VD

  def pi: VD

  def B: MD

  def b: Seq[Double]
}

sealed class Model(param:Parameters) extends ModelTrait{

  protected val T: MD = diag(pow(param.pi, 0.5))

  protected val Ti: MD = diag(pow(param.pi, -0.5))

  val pi: VD = param.pi

  val B: MD=
    DenseMatrix(
      (0.0    , param.a, param.b, param.c),
      (param.a, 0.0    , param.d, param.e),
      (param.b, param.d, 0.0    , param.f),
      (param.c, param.e, param.f, 0.0    ))

  val b:Seq[Double] = for(i <- 0 to 2; j<-i+1 to 3) yield B(i, j)

  //the (i,j) element is mutation rate i to j
  val R: MD =
    DenseMatrix(
      (0.0                  , param.a * param.pi(1), param.b * param.pi(2), param.c * param.pi(3)),
      (param.a * param.pi(0), 0.0                  , param.d * param.pi(2), param.e * param.pi(3)),
      (param.b * param.pi(0), param.d * param.pi(1), 0.0                  , param.f * param.pi(3)),
      (param.c * param.pi(0), param.e * param.pi(1), param.f * param.pi(2), 0.0                  ))

  for(i <- 0 to 3){R(i,i) = 0.0 - sum(R(i, ::).t)}

  for(i <- 0 to 3){B(i,i) = R(i,i) / pi(i)}

  protected val tmp: MD = T * R * Ti
  for(i <- 0 to 2; j <- i+1 to 3){tmp(j, i) = tmp(i, j)}
  val (lambda: VD, eVecs: MD) = eigSym(tmp)

  //R == u * diag(lambda) * ui
  val u: MD = Ti * eVecs

  val ui: MD = inv(u)

  def mstep(ns: VD, Ns: List[MD], Fd: List[VD], branches: List[Double]): (List[Double], Parameters) = {
    val Td: VD = (Fd, branches).zipped.map(_ * _).reduce(_ + _)
    val NS = Ns.reduce(_ + _)
    (newT(Ns, Fd), Parameters(newB(NS, Td), newPi(NS, Td, ns)))
  }

  protected def newPi(Ns:DenseMatrix[Double],Td:DenseVector[Double],ns:DenseVector[Double]) = {
    val u = (0 to 3) map (i => ns(i) + sum(Ns(::,i)) - Ns(i,i))
    val v = (0 to 3) map (i => (0 to 3).foldLeft(0.0)((x,j) => if(i != j) x + B(j,i) * Td(j) else x))
    calcNewParameter(u.toList,v.toList)
  }

  protected def newB(Ns:DenseMatrix[Double],Td:DenseVector[Double]) = {
    val u = for(i <- 0 to 2;j <- i+1 to 3) yield Ns(i,j) + Ns(j,i)
    val v = for(i <- 0 to 2;j <- i+1 to 3) yield pi(j) * Td(i) + pi(i) * Td(j)
    calcNewParameter(u.toList,v.toList)
  }

  protected def newT(Ns:List[DenseMatrix[Double]],Fd:List[DenseVector[Double]]):List[Double] = {
    val Ns0:List[Double] = Ns.map(x => sum(x) - trace(x))
    (Ns0,Fd).zipped.map((ns0,fd) => - ns0 / (0 to 3).foldLeft(0.0)((x,i) => x + R(i,i) * fd(i)))
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
}

object Model {
  def apply(param:Parameters) = new Model(param)
}