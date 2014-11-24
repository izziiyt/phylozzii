import breeze.linalg.{DenseMatrix, DenseVector, diag}
import scala.math._

abstract class Content(var t:Double){

  val alpha = DenseVector.zeros[Double](4)
  val beta = DenseVector.zeros[Double](4)
  val posterior = DenseMatrix.zeros[Double](4,4)
  private var transProb = DenseMatrix.zeros[Double](4,4)

  def format(){
    alpha(0 to 3) := 0.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
  }

  def accumInsideBelief(m:EvolutionModel) = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)) ((x,j) => x + alpha(j) * transProb(i,j)))
    DenseVector(tmp.toArray)
  }

  def accumOutsideBelief(m:EvolutionModel) = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)) ((x,j) => x + beta(j) * transProb(j,i)))
    DenseVector(tmp.toArray)
  }

  def setPosterior(likelihood:Double,m:EvolutionModel){
    for(i <- 0 to 3;j <- 0 to 3)
      posterior(i,j) = alpha(j) * beta(i) * transProb(i,j) / likelihood
  }

  def setTransProb(m:EvolutionModel){
    val tmp:DenseMatrix[Double] = diag(m.lambda.map(x => math.exp(x * t)))
    transProb = m.u * tmp * m.ui
  }

  @deprecated
  def manipulateTransition(m:DenseMatrix[Double]){
    transProb = m
  }

  //vector of Fd(i,C,theta)
  def FdVec(m:EvolutionModel) = {
    def fd(i:Int) = for(x <- 0 to 3;y<- 0 to 3) yield kai(x,y,i,i,m) * posterior(x,y)
    val tmp = (0 to 3) map (i => fd(i).sum)
    new DenseVector(tmp.toArray)
  }

  //Matrix of Ns(i,j,C,theta) i -> j
  def NsMat(m:EvolutionModel) = {
    def ns(i:Int,j:Int) = for(x <- 0 to 3;y <- 0 to 3) yield kai(x,y,i,j,m) * posterior(x,y)
    val tmp = for(j <- 0 to 3;i <- 0 to 3) yield ns(i,j).sum * m.R(i,j) * t
    new DenseMatrix(4,4,tmp.toArray)
  }

  @deprecated
  def NsMati(a:Int,b:Int,m:EvolutionModel):DenseMatrix[Double] = {
    val tmp = DenseMatrix.zeros[Double](4,4)
    for(i <- 0 to 3;j <- 0 to 3;if i != j){tmp(i,j) = m.R(i,j) * t * kai(a,b,i,j,m)}
    tmp
  }

  @deprecated
  def FdVeci(a:Int,b:Int,m:EvolutionModel):DenseVector[Double] = {
    val tmp = DenseVector.zeros[Double](4)
    for(i <- 0 to 3){tmp(i) = kai(a,b,i,i,m)}
    tmp
  }

  //beg -> end and from -> to
  private def kai(beg:Int,end:Int,from:Int,to:Int,m:EvolutionModel) = {
    def k(x:Double,y:Double) = if(DoubleChecker(x,y)) exp(x) else (exp(x) - exp(y)) / (x - y)
    val tmp = for(x <- 0 to 3; y <- 0 to 3) yield m.u(beg,x) * m.ui(x,from) * m.u(to,y) * m.ui(y,end) * k(t*m.lambda(x),t*m.lambda(y))
    tmp.sum / transProb(beg,end)
  }

  def likelihood(m:EvolutionModel):Double = alpha.t * m.pi
}

case class ContentOfLeaf(var tx:Double,var nuc:Char) extends Content(tx){
  override def format(){
    alpha(0 to 3) := 0.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
    nuc = 4
  }
}

case class ContentOfRoot(var tx:Double = 0.0) extends Content(tx){
  override def setPosterior(likelihood:Double,m:EvolutionModel){
    for(i <- 0 to 3)
      posterior(i,i) = alpha(i) * m.pi(i) / likelihood
  }
}

case class ContentOfNode(var tx:Double) extends Content(tx)