package main

import java.io._
import java.util.zip.GZIPInputStream
import alignment.Base
import biformat.{WigIterator, BedIterator}
import breeze.linalg.DenseVector
import breeze.numerics._
import breeze.plot._

import scala.io.Source

object Others{
  def wighist(wig: File, bed: File, chr: String, out: PrintStream = System.out) {
    val beds = biformat.bigSource(bed)
    val wigs = biformat.bigSource(wig)
    try {
      val bedit = BedIterator.fromSource(beds).filter(_.chr == chr)
      val wigit = if(bed.isFile) WigIterator.fromSource(wigs).filterWithBed(bedit) else  WigIterator.fromSource(wigs)
      out.println(wigit.hist(1000, 1).mkString(","))
    }
    finally {
      beds.close()
      wigs.close()
    }
  }

  def counter(f: File, out: PrintStream = System.out):Unit = {
    val counts = Array.fill[Long](5)(0)
    val s =
      if(f.getName.endsWith(".gz"))
        new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(f))))
      else
        new BufferedReader(new InputStreamReader(new FileInputStream(f)))
    var line = s.readLine()
    while(line != null){
      if (line.length > 0 && line.head == 's') {
        val xs = line.split("\\s+")(6)
        xs.toCharArray.foreach(x => counts(Base.fromChar(x).toInt) += 1)
      }
      line = s.readLine()
    }
    out.println(counts.mkString("\t"))
  }

  def entrop(inf: File) {
    val lines = Source.fromFile(inf).getLines().filter(l => !l.startsWith(">")).toArray
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
    val name = inf.getName.split('/').last.split('.').head
    p.title = name
    p.xlabel = "index"
    p.ylabel = "entropy"
    f.saveas("target/" + name + ".png")
  }

}