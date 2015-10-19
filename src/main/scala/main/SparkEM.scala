package main

import breeze.linalg.{DenseVector,DenseMatrix}
import org.apache.spark.{AccumulatorParam, SparkConf, SparkContext, Logging}
import alignment.Base
import fdur._

object SparkEM extends  Logging{

  def main(args: Array[String]) {
    logInfo("starting now")
    val sparkConf = new SparkConf().setAppName("SparkCountBases")
    sparkConf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    val sc = new SparkContext(sparkConf)
    var model = Model(Parameters.fromFile(args(1)))
    var tree = ModelTree.fromFile(args(2))
    val mafdata = Maf.readMaf(args(0),100)
    val cols = sc.parallelize(mafdata).cache()

    (0 until args(3).toInt).foreach{
      i =>
        //val model = Model(param)
        logInfo(i.toString)
        val mapped = cols.map { c =>
          val tmpt = tree
          val tmpm = model
          Eresult.fromTuple(Optimizer.ldestep(tmpt, c, tmpm))
        }
        val accum = sc.accumulator(Eresult.zero(tree.branches.length))(MyAccumulator)
        mapped.foreach(x => accum += x)
        /*val (rootns, ns, fd, lgl, n) = mapped.reduce {
          (m, x) =>
            logInfo(x._4.toString)
            val ns = m._1 + x._1
            val Ns = (m._2, x._2).zipped.map(_ + _)
            val Fd = (m._3, x._3).zipped.map(_ + _)
            val l = m._4 + x._4
            val i = m._5 + x._5
            (ns, Ns, Fd, l, i)
        }*/
        val x = accum.value
        val nd = x.b.toDouble
        //val nd = n.toDouble
        val (br, pr) = model.mstep(x.x / nd, x.ys.map(_ / nd), x.zs.map(_ / nd), tree.branches)
        model= Model(pr)
        tree = tree.changeBranches(br)
    }

    println(model.pi)
    println(model.b)
    println(tree.branches)

    //val lines = sc.textFile(args(0))
    //val data = lines.filter(_.startsWith("s")).map(_.split("\\s+")(6))
    //val result = data.map(countBases).reduce{(xs, ys) => (xs,ys).zipped.map(_ + _)}
    //println(result.mkString(","))
  }

}

case class Eresult(x:VD,ys:List[MD],zs:List[VD],a:Double,b:Long) {
  def +(that:Eresult) = Eresult(
    that.x + x,
    (that.ys,ys).zipped.map(_ + _),
    (that.zs,zs).zipped.map(_ + _),
    that.a + a,
    that.b + b)
  def size = ys.size

}

object Eresult{
  def fromTuple(x:(VD,List[MD],List[VD],Double,Long)):Eresult = Eresult(x._1,x._2,x._3,x._4,x._5)
  def zero(n: Int):Eresult = {
    Eresult(
    DenseVector.zeros[Double](4),
    List.fill(n)(DenseMatrix.zeros[Double](4,4)),
    List.fill(n)(DenseVector.zeros[Double](4)),
    0.0,
    0L)
  }
}

object MyAccumulator extends AccumulatorParam[Eresult] {
  def zero(initialValue: Eresult) : Eresult = {
    Eresult.zero(initialValue.size)
  }
  def addInPlace(v1: Eresult, v2: Eresult): Eresult = {
    v1 + v2
  }
}
