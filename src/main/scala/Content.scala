import breeze.linalg.{DenseMatrix, DenseVector, diag}
import scala.math.exp

abstract class Content(var t:Double){

  val alpha = DenseVector.zeros[Double](4)
  val beta = DenseVector.zeros[Double](4)
  val posterior = DenseMatrix.zeros[Double](4,4)
  var isNull = false
  private var transProb = DenseMatrix.zeros[Double](4,4)

  def format(){
    alpha(0 to 3) := 0.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
    isNull = false
  }

  def accumInsideBelief(m:EvolutionModel): DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)) ((x,j) => x + alpha(j) * transProb(i,j)))
    DenseVector(tmp.toArray)
  }

  def accumOutsideBelief(m:EvolutionModel): DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)) ((x,j) => x + beta(j) * transProb(j,i)))
    DenseVector(tmp.toArray)
  }

  def setPosterior(likelihood:Double){
    for(i <- 0 to 3;j <- 0 to 3)
      posterior(i,j) = alpha(j) * beta(i) * transProb(i,j) / likelihood
  }

  def setPosteriorNull(likelihood:Double,b:DenseVector[Double]){
    for(i <- 0 to 3;j <- 0 to 3)
      posterior(i,j) = b(i) * transProb(i,j) / likelihood
  }

  def setTransProb(m:EvolutionModel){
    val tmp:DenseMatrix[Double] = diag(m.lambda.map(x => math.exp(x * t)))
    transProb = m.u * tmp * m.ui
  }

  def nsAndFd(m:EvolutionModel) = {
    val k1 = for(b <- 0 to 3;a <- 0 to 3;x = m.lambda(a)*t;y = m.lambda(b)*t) yield
    {val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp}
    val k2 = for(b <- 0 to 3;a <- 0 to 3) yield {for(x <- 0 to 3;y <- 0 to 3) yield
      m.u(x,a)*m.ui(b,y)*posterior(x,y)/transProb(x,y)}.sum
    val s = new DenseMatrix(4,4,(k1,k2).zipped.map(_ * _).toArray)
    val ns = for(j <- 0 to 3;i <- 0 to 3) yield
      {for(a <- 0 to 3;b <- 0 to 3) yield s(a,b)*m.u(j,b)*m.ui(a,i)}.sum * m.R(i,j) * t
    val fd = for(i <- 0 to 3) yield
      {for(a <- 0 to 3;b <- 0 to 3) yield s(a,b)*m.u(i,b)*m.ui(a,i)}.sum
    (new DenseVector[Double](fd.toArray),new DenseMatrix(4,4,ns.toArray))
  }

  def likelihood(m:EvolutionModel):Double = alpha.t * m.pi
}

case class ContentOfLeaf(var tx:Double,var nuc:Char) extends Content(tx){
  override def format(){
    alpha(0 to 3) := 0.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
    nuc = 4
    isNull = false
  }
}

case class ContentOfRoot(var tx:Double = 0.0) extends Content(tx)

case class ContentOfNode(var tx:Double) extends Content(tx)