package fdur

import alignment.Base
import breeze.linalg.{diag, DenseMatrix, DenseVector}
import breeze.optimize.{LBFGS, DiffFunction}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.parallel.mutable.ParArray

object Optimizer extends LazyLogging {
  type Column = Array[Base]

  protected def emLike(itemax: Int, treex: ModelRoot, colsx: ParArray[List[Array[Base]]], paramx: Parameters,
             estepFunc:(ModelRoot,List[Column],Model) => (VD,List[MD],List[VD],Double,Long)): (List[Double], Parameters) = {
    var tree = treex
    val cols = colsx
    var param = paramx
    var i = 0
    var converged = false
    var currentlgl = Double.NegativeInfinity
    while(i < itemax && !converged) {
      val model = Model(param)
      val suffs = cols.map(estepFunc(tree, _, model))
      val (lgl, br, pr) = mstep(suffs, model, tree.branches)
      converged =
        util.doubleEqual(br,tree.branches,1.0E-8) &&
        util.doubleEqual(pr.pi,param.pi,1.0E-8) &&
        util.doubleEqual(pr.Bvec,param.Bvec,1.0E-8)
      tree = tree.changeBranches(br)
      param = pr
      if(i > 1 && currentlgl > lgl) {
        logger.error("log likelihood decreased.")
        converged = true
      }
      currentlgl = lgl
      i += 1
    }
    logger.info("optimized_log_likelihood=" + currentlgl + " iteration=" + i)
    regularize(tree.branches, param)
  }

  def em(itemax: Int, treex: ModelRoot, colsx: ParArray[List[Array[Base]]], paramx: Parameters) =
    emLike(itemax, treex, colsx, paramx, estep)

  def ldem(itemax: Int, treex: ModelRoot, colsx: ParArray[List[Array[Base]]], paramx: Parameters) =
    emLike(itemax, treex, colsx, paramx, ldestep)

  protected def gdLike(maxit:Int, template:ModelRoot, cols:ParArray[List[Array[Base]]], iniparam:DenseVector[Double],
                        mapper:(ModelRoot,List[Array[Base]],Model) => (VD,MD,List[Double],Double)) = {
    //val cols = Maf.readMaf(maf, 1000).toParArray
    //val template = ModelTree.fromFile(nh)

    val f = new DiffFunction[VD] {
      def calculate(p: VD) = {
        val param = Parameters(p(0 to 9))
        val branch = p(10 until p.length).toArray.toList
        val tree = template.changeBranches(branch)
        val model = Model(param)
        val diffs = cols.map(mapper(tree, _, model))
        val (regb, regpi) = mkreg(param)
        val (nlgl, newp) = gradReduce(diffs, regb, regpi)
        (nlgl,newp)
      }
    }

    val lbfgs = new LBFGS[VD](maxIter = maxit, m = 3)
    //val iniparam = paramx
   // val iniparam = DenseVector.vertcat(b,pi,DenseVector(template.branches.toArray))
    val optparam = lbfgs.minimize(f, DenseVector.vertcat(iniparam,DenseVector(template.branches.toArray)))
    val param = Parameters(optparam(0 to 9))
    val brnch = optparam(10 until optparam.length).toArray.toList
    regularize(brnch,param)
  }

  def gd(maxit:Int, template:ModelRoot, cols:ParArray[List[Array[Base]]], iniparam:DenseVector[Double]) =
    gdLike(maxit, template, cols, iniparam, gradMap)

  def ldgd(maxit:Int, template:ModelRoot, cols:ParArray[List[Array[Base]]], iniparam:DenseVector[Double]) =
    gdLike(maxit, template, cols, iniparam, ldgradMap)

  protected def mkreg(p: Parameters) = {
    val pitmp = DenseMatrix.vertcat(p.pi.asDenseMatrix,p.pi.asDenseMatrix,p.pi.asDenseMatrix,p.pi.asDenseMatrix)
    val pi = diag(p.pi) * (DenseMatrix.eye[Double](4) - pitmp)
    val btmp = DenseMatrix.vertcat(p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix,
      p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix, p.Bvec.asDenseMatrix)
    val b = diag(p.Bvec) * (DenseMatrix.eye[Double](6) - btmp)
    (b, pi)
  }

  //protected def mkLog(lgl: Double, tree: ModelRoot, param: Parameters):String =
  //  "\n" + "loglikelihood=" + lgl + "\n" + tree.toString + "\n" + "pi=" + param.pi + "\n" + "b=" + param.Bvec

  def estep(pr:ModelRoot,cols:List[Column],model:Model): (VD,List[MD],List[VD],Double,Long) =
    Tree.suffStat(pr,model,cols)

  def ldestep(pr:ModelRoot,cols:List[Column],model:Model): (VD,List[MD],List[VD],Double,Long) =
    LDTree.suffStat(pr,model,cols)

  def mstep(suffs: ParArray[(VD, List[MD], List[VD], Double, Long)],model:Model, branches:List[Double]):
  (Double, List[Double], Parameters) = {
    val (rootns, ns, fd, lgl, n):(VD, List[MD], List[VD], Double, Long) = suffs.reduce
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

  protected def gradMap(pr:ModelRoot,cols:List[Array[Base]],model:Model): (VD,MD,List[Double],Double) = {
    val tree = Tree.inout(pr,model,cols)
    val pi = tree.diffWithPi.reduceLeft(_ + _)
    val b = tree.diffWithB.reduceLeft(_ + _)
    val br = tree.diffWithT.map(_.sum)
    val likelihood = tree.loglikelihood.sum
    (pi,b,br,likelihood)
  }

  def ldgradMap(pr:ModelRoot,cols:List[Array[Base]],model:Model): (VD,MD,List[Double],Double) = {
    val tree = LDTree.inout(pr,model,cols)
    val pi = tree.diffWithPi.reduceLeft(_ + _)
    val b = tree.diffWithB.reduceLeft(_ + _)
    val br = tree.diffWithT.map(_.sum)
    val likelihood = tree.loglikelihood.sum
    (pi,b,br,likelihood)
  }

  protected def gradReduce(grads:ParArray[(VD,MD,List[Double],Double)],breg:MD,pireg:MD)
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
