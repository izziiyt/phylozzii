package main

import java.io.{BufferedWriter, File, FileWriter}
import breeze.linalg.{DenseVector,DenseMatrix}
import org.apache.spark.{AccumulatorParam, SparkConf, SparkContext, Logging}
import fdur._

object SparkEM extends  Logging{

  import util.doubleEqual

  implicit def name2file(f:String): File  = new File(f)

  def main(args: Array[String]): Unit = {
    exe(args(0), args(1), args(2), args(3).toInt, args(4).toInt, args.length >= 6 && args(5) == "-constFreq")
  }

  def exe(mf: String, pf: String, tf: String, maxit: Int, jobsize: Int = 512, constFreq: Boolean = false): Unit ={
    val sparkConf = new SparkConf().setAppName("SparkEM")
    //sparkConf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    val sc = new SparkContext(sparkConf)
    var param = Parameters.fromFile(pf)
    var tree = ModelTree.fromFile(tf)
    //val cols = sc.textFile(args(0)).map{x => x.split(",").map{_.toCharArray.map(Base.fromChar)}.toList}.cache()
    val cols = sc.parallelize(Maf.readMaf(mf,jobsize)).cache()
    var i = 1
    var f = true
    val piw = new BufferedWriter(new FileWriter("pi.log"))
    val bw = new BufferedWriter(new FileWriter("b.log"))
    val branchw = new BufferedWriter(new FileWriter("branch.log"))
    val lglw = new BufferedWriter(new FileWriter("lgl.log"))

    try {
      while (i <= maxit && f) {
        val model = Model(param)
        logInfo("Starting " + i.toString + "th of iteration.")
        val mapped = cols.map { c =>
          val tmpt = tree
          val tmpm = model
          Eresult.fromTuple(Optimizer.ldestep(tmpt, c, tmpm))
        }
        val accum = sc.accumulator(Eresult.zero(tree.branches.length))(MyAccumulatorParam)
        mapped.foreach(x => accum += x)
        val x = accum.value
        val n = x.n.toDouble
        val (newbr, newpr) = model.mstep(x.ns / n, x.Ns.map(_ / n), x.Fd.map(_ / n), tree.branches)
        f = !isConverged(tree.branches, newbr, param.pi, newpr.pi, DenseVector(param.Bvec.toArray), newpr.Bvec)
        val (rbr, rpr) = Optimizer.regularize(newbr, newpr)
        piw.write(rpr.pi.toArray.mkString(","))
        piw.newLine()
        bw.write(rpr.Bvec.toArray.mkString(","))
        bw.newLine()
        branchw.write(rbr.mkString(","))
        branchw.newLine()
        lglw.write(x.lgl.toString)
        lglw.newLine()
        param = if(constFreq) Parameters(newpr.Bvec, param.pi) else newpr
        tree = tree.changeBranches(newbr)
        i += 1
      }
    }catch {
      case e: Throwable => e
    }finally {
      piw.close()
      bw.close()
      lglw.close()
      branchw.close()
    }
    logInfo(if(f) "Iteration number reached upper limit." else "Parameters are converged.")
    val (rbr, rpr) = Optimizer.regularize(tree.branches,param)
    println("pi\t" + param.pi.toArray.mkString(","))
    println("b\t" + param.Bvec.toArray.mkString(","))
    println("tree\t" + tree.toString)
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

object MyAccumulatorParam extends AccumulatorParam[Eresult] {
  def zero(initialValue: Eresult) : Eresult = {
    Eresult.zero(initialValue.size)
  }
  def addInPlace(v1: Eresult, v2: Eresult): Eresult = {
    v1 + v2
  }
}
