package fdur

import alignment.Base
import breeze.linalg.DenseVector

object EM {
  type Column = Array[Base]

  def estep(pr:ModelRoot,cols:List[Column],model:Model): (VD,List[MD],List[VD],Double,Int) = Tree.suffStat(pr,model,cols)

  def mstep(suffs: Array[(VD, List[MD], List[VD], Double, Int)],model:Model, branches:List[Double]):
  (Double, List[Double], Parameters) = {
    val (rootns, ns, fd, lgl, n):(VD, List[MD], List[VD], Double, Int) = suffs.reduce
    {
      (m, x) =>
        val ns = m._1 + x._1
        val Ns = (m._2, x._2).zipped.map(_ + _)
        val Fd = (m._3, x._3).zipped.map(_ + _)
        val l = m._4 + x._4
        val i = m._5 + x._5
        (ns, Ns, Fd, l, i)
    }
    val nd = n.toDouble
    val (br, pr) = model.mstep(rootns / nd, ns.map(_ / nd), fd.map(_ / nd), branches)
    (lgl, br, pr)
  }

  def gradMap(pr:ModelRoot,cols:List[Column],model:Model): (VD,MD,List[Double],Double) = {
    val tree = Tree.inout(pr,model,cols)
    val pi = tree.diffWithPi.reduceLeft(_ + _)
    val b = tree.diffWithB.reduceLeft(_ + _)
    val br = tree.diffWithT.map(_.sum)
    val likelihood = tree.loglikelihood.sum
    (pi,b,br,likelihood)
  }

  def gradReduce(grads:Array[(VD,MD,List[Double],Double)],param:Parameters,branch:List[Double],alpha:Double)
  : (Double,List[Double],Parameters) = {
    val (pi,b,br,lgl):(VD,MD,List[Double],Double) = grads.reduce {
      (x,y) =>
        val n1 = x._1 + y._1
        val n2 = x._2 + y._2
        val n3 = (x._3,y._3).zipped.map(_ + _)
        val n4 = x._4 + y._4
        (n1,n2,n3,n4)
    }
    println(lgl)
    val barray = {for(i <- 0 to 2;j <- i+1 to 3) yield b(i,j)}.toArray
    (lgl,(branch,br).zipped.map((x,y) => x + alpha*y),param + (Parameters(DenseVector[Double](barray),pi) * alpha))
  }


}
