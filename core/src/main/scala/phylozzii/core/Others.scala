package phylozzii.core

import java.io._
import java.util.zip.GZIPOutputStream
import alignment.Base
import biformat.WigIterator.VariableStep
import biformat.{WigIterator, BedIterator}
import biformat.BedIterator.BedLine
import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import scala.io.Source

object Others{
  def wighist(wigs: Source, beds: Option[Source], chr: String, out: PrintStream = System.out) {
    val bedit = beds map (b => BedIterator.fromSource(b).filter(_.chr == chr))
    val preWigit = WigIterator.fromSource(wigs, 2048)
    val wigit = bedit map preWigit.filterWithBed getOrElse preWigit
    out.println(wigit.hist(1000, 1).mkString(","))
  }

  def counter(s: Source, out: PrintStream = System.out):Unit = {
    val counts = Array.fill[Long](5)(0)
    for(line <- s.getLines){
      if (line.length > 0 && line.head == 's') {
        val xs = line.split("\\s+")(6)
        xs.toCharArray.foreach(x => counts(Base.fromChar(x).toInt) += 1)
      }
    }
    out.println(counts.mkString("\t"))
  }

  def entrop(s: Source, name: String) {
    val lines = s.getLines().filter(!_.startsWith(">")).toArray
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
    p.title = name
    p.xlabel = "index"
    p.ylabel = "entropy"
    f.saveas("target/" + name + ".png")
  }

  def bedChrSplit(bedSource: Source, odir: File = new File(".")): Unit = {
    val farray = scala.collection.mutable.Map[String, PrintWriter]()
    def choosePrinter(chr: String): PrintWriter = {
      farray(chr) =
        if(farray.contains(chr)) farray(chr)
        else new PrintWriter(new GZIPOutputStream(new FileOutputStream(odir + "/" + chr +  ".bed.gz"), 1024 * 1024))
      farray(chr)
    }
    def bedManage(chr: String, f: List[BedLine] => List[BedLine], name: String = ""): Unit = {
      val bed = new File(odir + "/" + chr +  ".bed.gz")
      val source = biformat.bigSource(bed)
      val tmpbed = new File("/tmp/" + chr +  ".tmp.bed.gz")
      //val tmpbed = new File(odir + "/" + chr +  ".tmp.bed.gz")
      val printer = new PrintWriter(new GZIPOutputStream(new FileOutputStream(tmpbed), 1024 * 1024))
      try {
        val bedList = BedIterator.fromSource(source).toList
        f(bedList) foreach printer.println
      }
      finally {
        source.close()
        printer.close()
        tmpbed.renameTo(bed)
      }
    }

    def bedRemoveOverlap(chr: String) =
      bedManage(chr, zs => zs.tail.foldLeft(zs.head :: Nil){(ns, n) => if(ns.head hasOverlap n) ns else n :: ns}.reverse)
    def bedSort(chr: String): Unit = bedManage(chr, zs => zs.sortBy(_.start))

    val bit = BedIterator.fromSource(bedSource)
    bit.foreach{it => choosePrinter(it.chr).println(it.toString())}
    farray.values.foreach(_.close())
    farray.keys.foreach(bedSort)
    farray.keys.foreach(bedRemoveOverlap)
  }

  def diff(blsf: File, blsaf: File, out: PrintStream = System.out): Unit = {
    val bls = biformat.bigSource(blsf)
    val blsa = biformat.bigSource(blsaf)
    val matrix = Array.fill[Array[Int]](1000)(Array.fill[Int](1000)(0))
    val it = WigIterator.fromSource(bls, 1000)
    val ait = WigIterator.fromSource(blsa, 1000)
    while(it.hasNext && ait.hasNext){
      (it.next(), ait.next()) match {
        case (VariableStep(_,_,xlines), VariableStep(_,_,ylines)) =>
          (xlines zip ylines) foreach {
            case (x, y) =>
              matrix((x._2 * 1000).toInt)((y._2 * 1000).toInt) += 1
          }
        case _ =>
          throw new UnsupportedEncodingException
      }
    }
    matrix.foreach(x => out.println(x.mkString(",")))
    bls.close()
    blsa.close()
  }

  def wigwig(ws1: Source, ws2: Source, of: PrintStream): Unit = {
    of.println("index, first, second")
    def printer(first: VariableStep, second: VariableStep): Unit = {
      val start = max(first.start, second.start)
      val end = min(first.end, second.end)
      val x1 = first.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      val x2 = second.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      (x1 zip x2).foreach{case (x, y) => of.println("$x._1, $x._2, $y._2")}
    }
    val wi1 = WigIterator.fromSource(ws1)
    val wi2 = WigIterator.fromSource(ws2)

    var w1 = wi1.next()
    var w2 = wi2.next()

    while(wi1.hasNext && wi2.hasNext){
      if (w1.start > w2.end) w2 = wi2.next()
      else if (w1.end < w2.start) w1 = wi1.next()
      else {
        printer(w1.toVariableStep, w2.toVariableStep)
        if(w1.end < w2.end) w1 = wi1.next()
        else w2 = wi2.next()
      }
    }
    if(w1.start <= w2.end && w1.end >= w2.start)
      printer(w1.toVariableStep, w2.toVariableStep)
  }

}