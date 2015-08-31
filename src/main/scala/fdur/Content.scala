package fdur

import breeze.linalg.{diag, DenseMatrix, DenseVector}
import breeze.numerics.exp

abstract class Content(var t:Double){

  val alpha = DenseVector.ones[Double](4)
  val beta = DenseVector.zeros[Double](4)
  val posterior = DenseMatrix.zeros[Double](4,4)
  var isNull = false
  //the (i,j) element is mutation speed i to j
  private var transProb = DenseMatrix.zeros[Double](4,4)

  def format(){
    alpha(0 to 3) := 1.0
    beta(0 to 3) := 0.0
    posterior(0 to 3,0 to 3) := 0.0
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
    transProb = m.u * diag(exp(m.lambda * t)).*(m.ui)
  }

  def nsAndFd(m:EvolutionModel) = {
    val k1 = for(v <- 0 to 3;u <- 0 to 3;x = m.lambda(u)*t;y = m.lambda(v)*t) yield
    {val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp}
    val k2 = for(v <- 0 to 3;u <- 0 to 3) yield {for(st <- 0 to 3;en <- 0 to 3) yield
      m.u(st,u)*m.ui(v,en) * posterior(st,en) / transProb(st,en)}.sum
    val s = new DenseMatrix(4,4,(k1,k2).zipped.map(_ * _).toArray)
    val ns = for(j <- 0 to 3;i <- 0 to 3) yield
      {for(u <- 0 to 3;v <- 0 to 3) yield s(u,v)*m.u(j,v)*m.ui(u,i)}.sum * m.R(i,j) * t
    val fd = for(i <- 0 to 3) yield
      {for(u <- 0 to 3;v <- 0 to 3) yield s(u,v)*m.u(i,v)*m.ui(u,i)}.sum
    (new DenseVector[Double](fd.toArray),new DenseMatrix(4,4,ns.toArray))
  }

  def likelihood(m:EvolutionModel):Double = alpha.t * m.pi


  //for test Ns(a,b,i,j)------------------------------------------------------------------------------------------------
  //NsPremitive(a,b) => DenseMatrix(i,j)
  def nsPremitive(m:EvolutionModel):Array[Array[DenseMatrix[Double]]] = {
    def k(x:Double,y:Double) = {val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp}
    {
      for (start <- 0 to 3) yield {
        for (end <- 0 to 3) yield {
          val tmp1 = for (to <- 0 to 3; from <- 0 to 3) yield {
            {
              for (u <- 0 to 3; v <- 0 to 3)
                yield m.u(start,u) * m.ui(u, from) * m.u(to, v) * m.ui(v, end) *
                  k(t * m.lambda(u), t * m.lambda(v)) * m.R(from, to).*(t)
            }.sum
          }
          new DenseMatrix[Double](4, 4, tmp1.toArray) / transProb(start,end)
        }
      }.toArray
    }.toArray
  }

  def fdPremitive(m:EvolutionModel):Array[Array[DenseVector[Double]]] = {
    def k(x:Double,y:Double) = {val tmp = (exp(x) - exp(y)) / (x - y); if(tmp.isNaN) exp(x) else tmp}
    {
      for (start <- 0 to 3) yield {
        for (end <- 0 to 3) yield {
          val tmp1 = for (tofrom <- 0 to 3) yield {
            {
              for (u <- 0 to 3;v <- 0 to 3)
                yield m.u(start,u) * m.ui(u, tofrom) * m.u(tofrom, v) * m.ui(v, end) * k(t * m.lambda(u), t * m.lambda(v))
            }.sum
          }
          new DenseVector[Double](tmp1.toArray) / transProb(start,end)
        }
      }.toArray
    }.toArray
  }
  //for test------------------------------------------------------------------------------------------------------------

}
