package main

import java.io._
import biformat.MafIterator
import breeze.linalg.{DenseVector,DenseMatrix}
import org.apache.spark.{AccumulatorParam, SparkConf, SparkContext, Logging}
import fdur._
import biformat.Maf.readMaf

object SparkFdur extends Logging{

  import util.doubleEqual

  def sparkem(mf: File, pf: File, tf: File, maxit: Int = 1000, onejobsize: Int = 512, constFreq: Boolean = false): Unit ={

    val sparkConf = new SparkConf().setAppName("SparkEM")
    val sc = new SparkContext(sparkConf)

    val piw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("pi.log")))
    val bw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("b.log")))
    val branchw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("branch.log")))
    val lglw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("lgl.log")))

    def logging(pi:VD, Bvec: VD, br: List[Double], lgl: Double) = {
      piw.println(pi.toArray.mkString(","))
      bw.println(Bvec.toArray.mkString(","))
      branchw.println(br.mkString(","))
      lglw.println(lgl.toString)
    }

    //def readMaf(mf: String, per: Int = 512): Array[List[Array[Base]]] = {
    val source = biformat.bigSource(mf)
    val its = MafIterator.fromMSA(source, "hg19")
    val cols = sc.parallelize(its.merge(10000).its.map(_.seqs).toArray)

    var i = 1
    var f = true
    var param = Parameters.fromFile(pf)
    var tree = ModelTree.fromFile(tf)
   // val cols = sc.parallelize(readMaf(mf.getPath, onejobsize)).cache()

    try {
      while (i <= maxit && f) {
        val model = Model(param)
        logInfo("Starting " + i.toString + "th of iteration.")
        val mapped = cols.map { c =>
          val tmpt = tree
          val tmpm = model
          Eresult.fromTuple(Optimizer.ldestep(tmpt, c, tmpm))
        }
        val accum = sc.accumulator(Eresult.zero(tree.branches.length))(FdurAccumulatorParam)
        mapped.foreach(x => accum += x)
        val x = accum.value
        val n = x.n.toDouble
        val (newbr, newpr) = model.mstep(x.ns / n, x.Ns.map(_ / n), x.Fd.map(_ / n), tree.branches)
        f = !isConverged(tree.branches, newbr, param.pi, newpr.pi, DenseVector(param.Bvec.toArray), newpr.Bvec)
        val (rbr, rpr) = Optimizer.regularize(newbr, newpr)

        logging(rpr.pi, rpr.Bvec, rbr, x.lgl)

        param = if(constFreq) Parameters(newpr.Bvec, param.pi) else newpr
        tree = tree.changeBranches(newbr)
        i += 1
      }
    }catch {
      case e: Throwable => e.printStackTrace()
    }finally {
      piw.close()
      bw.close()
      lglw.close()
      branchw.close()
    }
    logInfo(if(f) "Iteration number reached upper limit." else "Parameters are converged.")
    val (rbr, rpr) = Optimizer.regularize(tree.branches, param)
    println("pi\t" + rpr.pi.toArray.mkString(","))
    println("b\t" + rpr.Bvec.toArray.mkString(","))
    println("tree\t" + tree.changeBranches(rbr))
  }

  def isConverged(br:List[Double], newbr: List[Double],
                  pi: DenseVector[Double], newpi: DenseVector[Double],
                  b: DenseVector[Double], newb: DenseVector[Double]) = {
    doubleEqual(br, newbr, 1.0E-7) && doubleEqual(pi, newpi, 1.0E-5) && doubleEqual(b, newb, 1.0E-5)
  }
}

case class Eresult(ns: VD, Ns: List[MD], Fd: List[VD], lgl: Double, n: Long) {
  def +(that:Eresult) = Eresult(
    that.ns + ns,
    (that.Ns,Ns).zipped.map(_ + _),
    (that.Fd,Fd).zipped.map(_ + _),
    that.lgl + lgl,
    that.n + n)
  def size = Ns.size
}

object Eresult{
  def fromTuple(x:(VD,List[MD],List[VD],Double,Long)):Eresult = Eresult(x._1,x._2,x._3,x._4,x._5)
  def zero(n: Int):Eresult = {
    Eresult(
    DenseVector.zeros[Double](4),
    List.fill(n)(DenseMatrix.zeros[Double](4,4)),
    List.fill(n)(DenseVector.zeros[Double](4)),
    0.0, 0L)
  }
}

object FdurAccumulatorParam extends AccumulatorParam[Eresult] {
  def zero(initialValue: Eresult) : Eresult = Eresult.zero(initialValue.size)
  def addInPlace(v1: Eresult, v2: Eresult): Eresult = v1 + v2
}
