import breeze.linalg.{DenseMatrix,DenseVector,diag,sum,eigSym,inv}

abstract class EvolutionModel{
  def R:DenseMatrix[Double]
  def u:DenseMatrix[Double]
  def ui:DenseMatrix[Double]
  def lambda:DenseVector[Double]
  def pi:DenseVector[Double]
  def B:DenseMatrix[Double]
  def param:Parameters
}

case class GTR(param:Parameters = Parameters(DenseVector[Double](1.0/12.0,2.0/12.0,3.0/12.0,1.0/12.0,2.0/12.0,3.0/12.0),
    DenseVector[Double](0.1,0.2,0.3,0.4))) extends EvolutionModel{

  private[this] val T:DenseMatrix[Double] = diag(param.pi.map(math.pow(_,0.5)))
  private[this] val Ti:DenseMatrix[Double] = diag(param.pi.map(math.pow(_,-0.5)))

  val B =
    DenseMatrix(
      (0.0,param.a,param.b,param.c),
      (param.a,0.0,param.d,param.e),
      (param.b,param.d,0.0,param.f),
      (param.c,param.e,param.f,0.0))

  val R =
    DenseMatrix(
      (0.0,param.a * param.pi(1),param.b * param.pi(2),param.c * param.pi(3)),
      (param.a * param.pi(0),0.0,param.d * param.pi(2),param.e * param.pi(3)),
      (param.b * param.pi(0),param.d * param.pi(1),0.0,param.f * param.pi(3)),
      (param.c * param.pi(0),param.e * param.pi(1),param.f * param.pi(2),0.0))

  for(i <- 0 to 3){R(i,i) = 0.0 - sum(R(i,::).t)}

  for(i <- 0 to 3){B(i,i) = R(i,i) / pi(i)}

  val tmp:DenseMatrix[Double] = T * R * Ti
  for(i <- 0 to 3;j <- i to 3){tmp(j,i) = tmp(i,j)}
  val (lambda:DenseVector[Double],eVecs:DenseMatrix[Double]) = eigSym(tmp)

  val u = Ti * eVecs
  val ui = inv(u)
  def pi = param.pi
}