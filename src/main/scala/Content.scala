/**
 * Created by yuto on 14/08/18.
 */
class Content {
  val alpha = Array.fill(4)(0.0)
  val beta = Array.fill(4)(0.0)
  val postProb = Array.ofDim[Double](4,4)
  var t:Double = 1.0

  def accumInsideBelief =
    (0 until 4) map (i => (0.0 /: (0 until 4)) ((x,j) => x + alpha(j) * GTR.transProb(i,j,t)))

  def accumOutsideBelief =
    (0 until 4) map (i => (0.0 /: (0 until 4)) ((x,j) => x + beta(j) * GTR.transProb(j,i,t)))

  def refactPostProbability(likelihood:Double){
    for(i <- 0 until 4;j <- 0 until 4)
      postProb(i)(j) = alpha(i) * beta(j) * GTR.transProb(i,j,t) / likelihood
  }

  def treeProb = ((alpha,GTR.pi).zipped map (_ * _)).sum
}