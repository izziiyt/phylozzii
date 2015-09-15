package fdur

import breeze.linalg.DenseVector
import alignment.Base

object GD {
  def gradMap(pr:ModelRoot,cols:List[Array[Base]],model:Model): (VD,MD,List[Double],Double) = {
    val tree = Tree.inout(pr,model,cols)
    val pi = tree.diffWithPi.reduceLeft(_ + _)
    val b = tree.diffWithB.reduceLeft(_ + _)
    val br = tree.diffWithT.map(_.sum)
    val likelihood = tree.loglikelihood.sum
    (pi,b,br,likelihood)
  }

  def gradReduce(grads:Array[(VD,MD,List[Double],Double)],breg:VD,pireg:VD)
  : (Double,VD) = {
    val (pi, b, br, lgl): (VD, MD, List[Double], Double) = grads.reduce {
      (x, y) =>
        val n1 = x._1 + y._1
        val n2 = x._2 + y._2
        val n3 = (x._3, y._3).zipped.map(_ + _)
        val n4 = x._4 + y._4
        (n1, n2, n3, n4)
    }
    val barray = {for (i <- 0 to 2; j <- i + 1 to 3) yield b(i, j)}.toArray
    val bgrad = (DenseVector(barray) :* breg).toArray
    val pigrad = (pi :* pireg).toArray
    val brgrad = br.toArray
    (-lgl, -DenseVector[Double](bgrad ++ (pigrad ++ brgrad)))
  }
}
