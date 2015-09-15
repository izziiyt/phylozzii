package fdur

import alignment.Base
import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging

object EM extends LazyLogging {
  type Column = Array[Base]

  def exe(itemax: Int, nhf: String, maf: String): (List[Double], Parameters) = {
    val pi = Array(0.25469972246441502, 0.2452686122063337, 0.24531127848266232, 0.25472038684658888)
    val b = Array(0.81053441227174539, 2.4236183781739533, 0.65677131469221517, 0.88544145555567511, 2.4233580444379776, 0.8106412600752263)
    var tree = ModelTree.fromFile(nhf)
    val cols = Maf.readMaf(maf, 1000)
    var param = Parameters(DenseVector(b), DenseVector(pi))
    var i = 0
    var check = true
    var currentlgl:Double = Double.NegativeInfinity
    while(i < itemax && check) {
      val model = Model(param)
      val suffs = cols.map(EM.estep(tree, _, model))
      val (lgl, br, pr) = EM.mstep(suffs, model, tree.branches)
      tree = tree.changeBranches(br)
      param = pr
      check = !doubleEqual(currentlgl,lgl)
      currentlgl = lgl
      i += 1
      //logger.info(mkLog(lgl,tree,param))
    }
    logger.info("\n" + "optimized_log_likelihood=" + currentlgl + "\n" + "iteration=" + i + "\n")
    regularize(tree.branches, param)
  }

  protected def mkLog(lgl: Double, tree: ModelRoot, param: Parameters):String =
    "\n" + "loglikelihood=" + lgl + "\n" + tree.toString + "\n" + "pi=" + param.pi + "\n" + "b=" + param.Bvec


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
  def gradMap(pr:ModelRoot,cols:List[Array[Base]],model:Model): (VD,MD,List[Double],Double) = {
    val tree = Tree.inout(pr,model,cols)
    val pi = tree.diffWithPi.reduceLeft(_ + _)
    val b = tree.diffWithB.reduceLeft(_ + _)
    val br = tree.diffWithT.map(_.sum)
    val likelihood = tree.loglikelihood.sum
    (pi,b,br,likelihood)
  }

  def gradReduce(grads:Array[(VD,MD,List[Double],Double)],breg:MD,pireg:MD)
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
    val bgrad:DenseVector[Double] = breg * DenseVector[Double](barray)
    val pigrad = pireg * pi
    val brgrad = DenseVector[Double](br.toArray)
    (-lgl, -DenseVector.vertcat(bgrad,pigrad,brgrad))
  }

  def regularize(brnc:List[Double],param:Parameters): (List[Double],Parameters) = {
    val model = Model(param)
    val summ = - (0 to 3).foldLeft(0.0){(x,i) => x + model.pi(i) * model.R(i,i)}
    val br = brnc map (_ * summ)
    val b = param.Bvec / summ
    (br,Parameters(b,param.pi))
  }

}
