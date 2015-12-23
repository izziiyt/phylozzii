package main
import breeze.linalg._
import breeze.plot._
import alignment.Base
import breeze.numerics.log

import scala.io.Source

/**
  * Created by yuto on 15/12/17.
  */
object Entrop {
  def main(args:Array[String]) {
    val lines = Source.fromFile(args(0)).getLines().filter(l => !l.startsWith(">")).toArray
    val tlines = lines.head.indices.map(i => lines.map(_ (i)))
    val counts = tlines.map {
      cols =>
        val tmp = Array(0, 0, 0, 0)
        cols.foreach(
          Base.fromChar(_) match {
            case Base.N | Base.D => (0 to 3).foreach{tmp(_) +=1}
            case y => tmp(y.toInt) += 1
          }
        )
        tmp
    }
    val z = counts.map {
      xs =>
        val n = xs.sum
        xs.map { x => x * (log(n) - log(x)) }.sum / n
    }.toArray

    val f = Figure()
    f.visible = false
    val p = f.subplot(0)
    val x:DenseVector[Double] = DenseVector(z.indices.map(_.toDouble).toArray)
    val y:DenseVector[Double] = DenseVector(z)
    p += plot(x,y)
    val name = args(0).split('/').last.split('.').head
    p.title = name
    p.xlabel = "index"
    p.ylabel = "entropy"
    f.saveas("target/" + name + ".png")
  }

}
