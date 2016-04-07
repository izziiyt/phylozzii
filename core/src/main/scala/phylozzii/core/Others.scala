package phylozzii.core

import java.io._
import java.util.zip.GZIPOutputStream

import alignment.Base
import biformat.WigIterator.VariableStep
import biformat.{BedIterator, WigIterator}
import biformat.BedIterator.BedLine
import breeze.linalg._
import breeze.numerics._
import breeze.plot._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Sorting

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
      val tmpbed = new File(s"/tmp/$chr.tmp.bed.gz")
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

  def wigwigphyloP(ws1: Source, ws2: Source, prnt: PrintStream): Unit = {
    val SIZE = 100
    val mat = DenseMatrix.zeros[Int](SIZE, SIZE)

    def save(first: VariableStep, second: VariableStep): Unit = {
      val start = max(first.start, second.start)
      val end = min(first.end, second.end)
      val x1 = first.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      val x2 = second.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      (x1 zip x2).foreach{
        case (x, y) =>
          val y2 = ((if(y._2 < 0) max(-15.0, y._2) else min(15.0, y._2)) + 15.0) / 30.0
          mat(min(99, (x._2 * SIZE).toInt), min(99, (y2 * SIZE).toInt)) += 1}
    }

    val wi1 = WigIterator.fromSource(ws1)
    val wi2 = WigIterator.fromSource(ws2)

    var w1 = wi1.next()
    var w2 = wi2.next()

    while(wi1.hasNext && wi2.hasNext){
      if (w1.start > w2.end) w2 = wi2.next()
      else if (w1.end < w2.start) w1 = wi1.next()
      else {
        save(w1.toVariableStep, w2.toVariableStep)
        if(w1.end < w2.end) w1 = wi1.next()
        else w2 = wi2.next()
      }
    }
    if(w1.start <= w2.end && w1.end >= w2.start) {
      save(w1.toVariableStep, w2.toVariableStep)
    }
    for(i <- 0 until mat.rows){
      prnt.println(mat(i, ::).t.toArray.mkString(","))
    }
  }

  def wigwig(ws1: Source, ws2: Source, prnt: PrintStream): Unit = {
    val SIZE = 100
    val mat = DenseMatrix.zeros[Int](SIZE, SIZE)

    def save(first: VariableStep, second: VariableStep): Unit = {
      val start = max(first.start, second.start)
      val end = min(first.end, second.end)
      val x1 = first.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      val x2 = second.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      (x1 zip x2).foreach{case (x, y) => mat(min(99, (x._2 * SIZE).toInt), min(99, (y._2 * SIZE).toInt)) += 1}
    }

    val wi1 = WigIterator.fromSource(ws1)
    val wi2 = WigIterator.fromSource(ws2)

    var w1 = wi1.next()
    var w2 = wi2.next()

    while(wi1.hasNext && wi2.hasNext){
      if (w1.start > w2.end) w2 = wi2.next()
      else if (w1.end < w2.start) w1 = wi1.next()
      else {
        save(w1.toVariableStep, w2.toVariableStep)
        if(w1.end < w2.end) w1 = wi1.next()
        else w2 = wi2.next()
      }
    }
    if(w1.start <= w2.end && w1.end >= w2.start) {
      save(w1.toVariableStep, w2.toVariableStep)
    }
    for(i <- 0 until mat.rows){
      prnt.println(mat(i, ::).t.toArray.mkString(","))
    }
  }

  /**
    * prints significant genes on an output stream
    *
    * @param inputSources
    * @param ps
    * @param minimum
    * @param maximum
    */
  def geneList(inputSources: Array[Source], ps: PrintStream, minimum: Double, maximum: Double) = {
    val names = scala.collection.mutable.Set.empty[String]
    inputSources.foreach{
      _.getLines().
        map(Enhancer(_)).
        dropWhile(_.score < minimum).
        takeWhile(_.score < maximum).
        foreach(_.gene.foreach(_.names.foreach(names += _)))
    }
    names.toArray.foreach(ps.println)
  }

  def enhancers(inputSource: Source, referenceSource: Source, ps: PrintStream) = {
    val enArray = BedIterator.fromSource(referenceSource).map(Enhancer(_)).toArray
    val enIt =
      enArray.sorted.
      foldLeft(Nil: List[Enhancer]){
      (zs,x) => if(zs.nonEmpty && zs.last == x) zs.head.merge(x) :: zs.tail else x :: zs
    }.reverse.toIterator

    val eblsIt = WigIterator.fromSource(inputSource)

    var enunit = enIt.next()
    var eblsunit = eblsIt.next()

    val buf = new ArrayBuffer[Enhancer]()
    val tmp = new ArrayBuffer[Double]()

    while (eblsIt.hasNext && enIt.hasNext) {
      if (eblsunit.chrom != enunit.chr || enunit.end <= eblsunit.start) {
        if(tmp.nonEmpty) {
          buf += enunit.withScore(tmp.sum / tmp.length)
          tmp.clear()
        }
        enunit = enIt.next()
      }
      else {
        if (enunit.start < eblsunit.end) {
          eblsunit.toVariableStep.lines.
            withFilter { case (x, _) => x < enunit.end && x >= enunit.start }.
            foreach { case (_, y) => tmp += y }
        }
        eblsunit = eblsIt.next()
      }
    }
    buf.sortBy(_.score).foreach{ps.println}
  }

  def bedSort(inputSource: Source, ps: PrintStream): Unit ={
    val bedar = BedIterator.fromSource(inputSource).toArray
    Sorting.stableSort(bedar, (x:BedLine, y: BedLine) => x.end < y.end)
    Sorting.stableSort(bedar, (x:BedLine, y: BedLine) => x.start < y.start)
    Sorting.stableSort(bedar, (x:BedLine, y: BedLine) => x.chr < y.chr)
    bedar foreach ps.println
  }

}

case class Enhancer(chr: String, start: Int, end: Int, gene: Array[Genes], score: Double) extends Ordered[Enhancer]{
  def merge(that: Enhancer): Enhancer = Enhancer(chr, start, end, this.gene ++ that.gene, this.score + that.score)
  override def compare(that: Enhancer): Int = {
    def chr2int(str: String): Int = {
      val suffix = str.diff("chr")
      try suffix.toInt catch {case _:Exception => suffix.head.toInt + 99}
    }
    val tmp = chr2int(this.chr) - chr2int(that.chr)
    if(tmp == 0) this.start - that.start
    else tmp
  }
  def withScore(x: Double): Enhancer = Enhancer(chr, start, end, gene, x)
  override def toString: String = s"$chr\t$start\t$end\t" + gene.mkString("~") + s"\t$score"
}

object Enhancer{
  val TSSLength = 401
  val EnhancerLength = 1001
  def apply(line: BedLine): Enhancer = {
    require(line.blockCount == 2)
    val start = line.start + line.blockStarts(if(line.blockSize.head == EnhancerLength) 0 else 1)
    new Enhancer(line.chr, start.toInt, start.toInt + EnhancerLength, Array(Genes(line.name)), 0.0)
  }

  def apply(str: String): Enhancer = {
    val args = str.split("""\p{javaWhitespace}""")
    val genes = args(3).split("~").map(Genes(_))
    new Enhancer(args.head, args(1).toInt, args(2).toInt, args(3).split("~").map(Genes(_)), args(4).toDouble)
  }
}

case class Genes(names: Array[String], R: String, FDR: String){
  override def toString: String = names.mkString(",") + s";$R;$FDR"
}

object Genes{
  def apply(str: String): Genes = {
    val args = str.split(";")
    val initialIndex = if(args(0).startsWith("chr")) 1 else 0
    Genes(args(initialIndex).split(","), args.init.last, args.last)
  }
}
