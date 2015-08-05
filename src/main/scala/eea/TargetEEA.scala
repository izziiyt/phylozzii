package eea

import breeze.linalg.DenseMatrix
import fdur._
import scala.collection.AbstractIterator
import scala.math._
/*
object TargetEEA {
  def singleEdgeEEA(em: EvolutionModel,t:Double):DenseMatrix[Double] = {
    val tmp = for(a <- 0 to 3;b <- 0 to 3) yield (0.0 /: (0 to 3)){
        (n,j) => n + em.u(a,j) * em.ui(j,b) * k(t * em.R(a,a),t * em.lambda(j))
      } * t / transProb(a,b)
    new DenseMatrix(4,4,tmp)
  }

  private def k(x:Double,y:Double):Double = {
    val tmp = (exp(x) - exp(y)) / (x - y)
    if(tmp.isNaN) exp(x) else tmp
  }

  def exe(name:String,tree:EEATree):Double = {
    val start = tree.find(start)
    start.eea()
  }

}

trait FileModuleStream[T] extends AbstractIterator[T] with Iterator[T] {
  def linesStream:Iterator[String]
  var nextValue:Option[T]
  def converter(x:Iterator[String]):Option[T]
  def next() = {
    val result = nextValue.get
    nextValue = converter(linesStream)
    result
  }
  def hasNext = nextValue.isDefined
}*/