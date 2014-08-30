/**
 * Created by yuto on 14/08/18.
 */

class Content(private var branch:Double = 1.0){
  val alpha = Array.fill(4)(0.0)
  val beta = Array.fill(4)(0.0)
  val postProb = Array.ofDim[Double](4,4)

  def reset(){
    for(i <- 0 until 4){
      alpha(i) = 0.0
      beta(i) = 0.0
      for(j <- 0 until 4)
        postProb(i)(j) = 0.0
    }
  }

  def t_=(x:Double){branch = x}

  def t = branch

  def accumInsideBelief =
    (0 until 4) map (i => (0.0 /: (0 until 4)) ((x,j) => x + alpha(j) * GTR.transProb(i,j,t)))

  def accumOutsideBelief =
    (0 until 4) map (i => (0.0 /: (0 until 4)) ((x,j) => x + beta(j) * GTR.transProb(j,i,t)))

  def refactPostProbability(likelihood:Double){
    for(i <- 0 until 4;j <- 0 until 4)
      postProb(i)(j) = alpha(j) * beta(i) * GTR.transProb(i,j,t) / likelihood
  }

  def calcLikelihood:Double = (alpha,GTR.pi).zipped.foldLeft(0.0){case (x,(a,p)) => x + a * p}

}