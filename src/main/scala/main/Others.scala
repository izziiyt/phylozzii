package main

import java.io._
import java.util.zip.GZIPOutputStream
import alignment.Base
import biformat.{WigIterator, BedIterator}
import biformat.BedIterator.BedLine
import breeze.linalg.DenseVector
import breeze.numerics._
import breeze.plot._
import scala.io.Source

object Others{
  def wighist(wig: File, bed: File, chr: String, out: PrintStream = System.out) {
    println("wighist activates")
    val wigs = biformat.bigSource(wig)
    if(bed.isFile){
      val beds = biformat.bigSource(bed)
      try {
        val bedit = BedIterator.fromSource(beds).filter(_.chr == chr)
        val wigit = WigIterator.fromSource(wigs, 2048).filterWithBed(bedit)
        out.println(wigit.hist(1000, 1).mkString(","))
      }
      catch{case e: Throwable => e.printStackTrace()}
      finally {
        beds.close()
        wigs.close()
      }
    }
    else{
      try {
        val wigit = WigIterator.fromSource(wigs, 2048)
        out.println(wigit.hist(1000, 1).mkString(","))
      }
      catch{case e: Throwable => e.printStackTrace()}
      finally {
        wigs.close()
      }
    }
  }

  def counter(f: File, out: PrintStream = System.out):Unit = {
    val counts = Array.fill[Long](5)(0)
    val s = biformat.bigSource(f)
    for(line <- s.getLines){
      if (line.length > 0 && line.head == 's') {
        val xs = line.split("\\s+")(6)
        xs.toCharArray.foreach(x => counts(Base.fromChar(x).toInt) += 1)
      }
    }
    out.println(counts.mkString("\t"))
  }

  def entrop(inf: File) {
    println("entrop activates")
    val lines = Source.fromFile(inf).getLines().filter(l => !l.startsWith(">")).toArray
    val tlines = lines.head.indices.map(i => lines.map(_ (i)))
    val counts = tlines.map {
      cols =>
        val tmp = Array(0, 0, 0, 0)
        cols.foreach(
          Base.fromChar(_) match {
            case Base.N | Base.D => (0 to 3).foreach{tmp(_) +=1}
            case b => tmp(b.toInt) += 1
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
    val x = DenseVector(z.indices.map(_.toDouble).toArray)
    val y = DenseVector(z)
    p += plot(x,y)
    val name = inf.getName.split('/').last.split('.').head
    p.title = name
    p.xlabel = "index"
    p.ylabel = "entropy"
    f.saveas("target/" + name + ".png")
  }

  def bedChrSplit(inf: File, odir: File = new File(".")): Unit = {
    val farray = scala.collection.mutable.Map[String, PrintWriter]()
    def mkpw(chr: String) = {
      if(!farray.contains(chr)){
        farray(chr) = new PrintWriter(new GZIPOutputStream(new FileOutputStream(odir + "/" + chr +  ".bed.gz"), 1024 * 1024))
      }
      farray(chr)
    }
    def bedManage(chr: String, f: List[BedLine] => List[BedLine], name: String = ""): Unit = {
      val source = biformat.bigSource(odir + "/" + chr +  ".bed.gz")
      val pw = new PrintWriter(new GZIPOutputStream(new FileOutputStream(odir + "/" + chr +  ".tmp.bed.gz"), 1024 * 1024))
      try {
        val tmp = BedIterator.fromSource(source).toList
        f(tmp) foreach pw.println
        new File(odir + "/" + chr +  ".tmp.bed.gz").renameTo(new File(odir + "/" + chr +  ".bed.gz"))
      }
      finally {
        source.close()
        pw.close()
      }
    }

    def bedRemoveOverlap(chr: String) =
      bedManage(chr, zs => zs.tail.foldLeft(zs.head :: Nil){(n, z) => if(n.head hasOverlap z) n else z :: n}.reverse)
    def bedSort(chr: String): Unit = bedManage(chr, zs => zs.sortBy(_.start))

    val source = biformat.bigSource(inf)
    val bit = BedIterator.fromSource(source)
    bit.foreach{it => mkpw(it.chr).println(it.toString())}
    source.close()
    farray.values.foreach(_.close())
    farray.keys.foreach(bedSort)
    farray.keys.foreach(bedRemoveOverlap)
  }

}