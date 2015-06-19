package eea

import alignment.Base
import breeze.linalg.{diag, DenseMatrix, DenseVector}
import fdur.EvolutionModel
import scala.math._

sealed trait EEAContent{

  def m:EvolutionModel
  val alpha = DenseVector.zeros[Double](4)
  val alphaD = DenseVector.zeros[Double](4)
  val beta = DenseVector.zeros[Double](4)
  val betaD = DenseVector.zeros[Double](4)
  val transProb = DenseMatrix.zeros[Double](4,4)
  val transProbD = DenseMatrix.zeros[Double](4,4)

  def insidePropagation:DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)){(x,j) => x + alpha(j) * transProb(i,j)})
    DenseVector(tmp.toArray)
  }

  def insidePropagationD:DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)){(x,j) => x + alphaD(j) * transProbD(i,j)})
    DenseVector(tmp.toArray)
  }

  def outPropagation:DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)){(x, j) => x + beta(j) * transProb(j, i) })
    DenseVector(tmp.toArray)
  }

  def outPropagationD:DenseVector[Double] = {
    val tmp = (0 to 3) map (i => (0.0 /: (0 to 3)){(x,j) => x + betaD(j) * transProbD(j,i)})
    DenseVector(tmp.toArray)
  }

}

case class EEARootContent() extends EEAContent
case class EEALeafContent(t:Double,base:Base) extends EEAContent
case class EEANodeContent(t:Double) extends EEAContent
