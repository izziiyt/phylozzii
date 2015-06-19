package fdur

import breeze.linalg.{DenseMatrix, DenseVector}

abstract class EvolutionModel{
  def R:DenseMatrix[Double]
  def u:DenseMatrix[Double]
  def ui:DenseMatrix[Double]
  def lambda:DenseVector[Double]
  def pi:DenseVector[Double]
  def B:DenseMatrix[Double]
  def param:Parameters
  def bList:Seq[Double]
}
