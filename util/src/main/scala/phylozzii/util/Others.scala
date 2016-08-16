package phylozzii.util

import java.io._
import java.util.zip.GZIPOutputStream

import biformat.Block
import alignment.Base
import biformat.WigIterator.{VariableStep, WigUnit}
import biformat.{BedIterator, WigIterator}
import biformat.BedIterator.BedLine
import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import EnhancerIterator._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Sorting

object Others extends {

  /**
    * make histgram with wig file
    *
    * @param wigf .wig file
    * @param bedf .bed file
    * @param out output path
    * @param enhancer manage .bed as .enh
    */
  def wighist(wigf: File, bedf: Option[File], out: File, enhancer: Boolean) {

    def f(wigf: File) {
      val beds = bedf map biformat.bigSource
      val bedit:Option[BedIterator] =
        if(enhancer) beds map (b => enhancerConcat(BedIterator.fromSource(b).map(Enhancer(_))).map(_.toBedLine))
        else beds map (b => BedIterator.fromSource(b))
      val prefix = wigf.getName.replace(".wig", "").replace(".gz", "")
      val outPath = new File(out.getAbsolutePath, prefix + ".hist")
      val wigs = biformat.bigSource(wigf)
      val os = new PrintWriter(new FileOutputStream(outPath))

      val preWigit = WigIterator.fromSource(wigs, 2048)
      val wigit = bedit map preWigit.filterWithBed getOrElse preWigit
      os.println(wigit.hist(1000, 1.0).mkString(","))

      wigs.close()
      os.close()
      beds.foreach(_.close)
    }

    if(wigf.isDirectory) wigf.listFiles.foreach(f) else f(wigf)

  }

  def length(file: File, mode: Int): Unit ={
    val s = Source.fromFile(file)
    val l = mode match {
      case 0 => enhancerConcat(BedIterator.fromSource(s).map(Enhancer(_))).length
      case 1 => BedIterator.fromSource(s).length
      case 2 => WigIterator.fromSource(s).length
      case _ => s.getLines().length
    }
    println(l)
  }

  def bedUnion(bedir: File, os: PrintStream): Unit ={
    val files = bedir.listFiles.filter(_.isFile)
    val beds = biformat.bigSource(files.head)
    val bedarr = BedIterator.fromSource(beds).filter(!_.chr.contains('_')).toArray
    beds.close()
    @tailrec
    def f(fs: Array[File], bedarr: Array[BedLine]): BedIterator = {
      if (fs.isEmpty) bedarr.toIterator
      else {
        val _beds = biformat.bigSource(fs.head)
        val _bedit = BedIterator.fromSource(_beds).filter(!_.chr.contains('_'))
        val tmp = _bedit.union(bedarr.toIterator).toArray
        _beds.close()
        f(fs.tail, tmp)
      }
    }
    val bedit = f(files.tail, bedarr)
    bedit.foreach(os.println)
  }

  def randomwig(wigf: File, os: PrintStream, num: Int): Unit ={
    val _wigs = biformat.bigSource(wigf)
    val MAX = WigIterator.fromSource(_wigs).length
    _wigs.close()

    val r = scala.util.Random
    val stairs = (0 until num).map{_ => r.nextInt(MAX)}.
      sortWith(_ > _).foldLeft(Nil:List[Int]){
      case (z::zs,x) => x :: z - x - 1 :: zs
      case (Nil,x) => x :: Nil
    }

    val wigs = biformat.bigSource(wigf)
    val wigit = WigIterator.fromSource(wigs).map(_.toVariableStep)

    var wig = wigit.next()

    for(i <- 1 to num) yield {
      wig = if(stairs(i - 1) == -1) wig else wigit.drop(stairs(i - 1)).next()
      os.println(wig)
    }
    wigs.close()
  }

  def randombed(bedf: File, out: File, num: Int): Unit ={
    val _beds = biformat.bigSource(bedf)
    val MAX = BedIterator.fromSource(_beds).count(!_.chr.contains('_')) - 1
    _beds.close()

    val r = scala.util.Random
    val stairs = (0 until num).map{_ => r.nextInt(MAX)}.
      sortWith(_ > _).foldLeft(Nil:List[Int]){
      case (z::zs,x) => x :: z - x - 1 :: zs
      case (Nil,x) => x :: Nil
    }

    val beds = biformat.bigSource(bedf)
    val os = new PrintStream(new FileOutputStream(out))
    val bedit = BedIterator.fromSource(beds).filter(!_.chr.contains('_'))

    var bed = bedit.next()

    for(i <- 1 to num) yield {
      bed = if(stairs(i - 1) == -1) bed else bedit.drop(stairs(i - 1)).next()
      os.println(bed)
    }

    os.close()
    beds.close()
  }

  def wigfilter(wf: File, bf: File, out: PrintStream, wing: Int, enhancer: Boolean, tss: Boolean = false): Unit = {
    val wigs = biformat.bigSource(wf)
    val beds = biformat.bigSource(bf)

    val bedit =
      if(enhancer) enhancerConcat(BedIterator.fromSource(beds).map(Enhancer(_))).
        map{case Enhancer(chr, start,end,_,_) => BedLine(chr, max(0, start - wing), end + wing)}
      else if(tss) tssConcat(BedIterator.fromSource(beds).map(TSS(_))).
        map{case TSS(chr, start,end,_) => BedLine(chr, max(0, start - wing), end + wing)}
      else BedIterator.fromSource(beds).
        map{case BedLine(chr,start,end,_,_,_,_,_,_,_,_,_) => BedLine(chr, max(0, start - wing), end + wing)}

    val wigit = WigIterator.fromSource(wigs).filterWithBed(bedit)

    wigit.foreach(out.println)

    wigs.close()
    beds.close()
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

  def wigwig(ws1: Source, ws2: Source, prnt: PrintStream, phylop: Boolean): Unit = {
    val SIZE = 100
    val mat = DenseMatrix.zeros[Int](SIZE, SIZE)

    def save(first: VariableStep, second: VariableStep): Unit = {
      val start = max(first.start, second.start)
      val end = min(first.end, second.end)
      val x1 = first.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      val x2 = second.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      require(x1.length == x2.length)
      if(phylop)
        (x1 zip x2).foreach{
          case (x, y) =>
            val y2 = ((if(y._2 < 0) max(-15.0, y._2) else min(15.0, y._2)) + 15.0) / 30.0
            mat(min(SIZE - 1, (x._2 * SIZE).toInt), min(99, (y2 * (SIZE - 1)).toInt)) += 1}
      else
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

  def goHist(gotsvf: File, bed: File, wigf: File, outdir: File, outfName: String): Unit = {
    val gotsvs = Source.fromFile(gotsvf)
    val gotsv = gotsvs.getLines().map{
      line =>
        val parts = line.split('\t')
        val go = parts(0) + ':' + parts(1).replace(' ', '_')
        val genes = parts(2).split(':').toSet
        (go, genes)
    }.filter{case (go, _) => go != "go:term"}
    for((go, genes) <- gotsv){
      println("now processing " + go + " with " + wigf.getName)
      val beds = biformat.bigSource(bed)
      val enhit = enhancerConcat(BedIterator.fromSource(beds).map(Enhancer(_))).
        collect{
          case enh if enh.gene.exists(_.names.toSet.intersect(genes).nonEmpty) => enh.toBedLine
        }
      val wigs = biformat.bigSource(wigf)
      val wigit = WigIterator.fromSource(wigs, 2048).filterWithBed(enhit)
      new File(outdir.getAbsolutePath, go).mkdir
      val ps = new PrintStream(new File(outdir.getAbsolutePath, go + "/" + outfName))
      ps.println(wigit.hist(1000, 1.0).mkString(","))
      ps.close()
      wigs.close()
      beds.close()
    }
    gotsvs.close()
  }

  def geneList(bfs: Array[File], wdir: File, of: File, minimum: Double, maximum: Double) = {
    val genes = scala.collection.mutable.Map.empty[String, Array[Int]]

    def addgenes(enh: Enhancer, wig: WigUnit): Unit = {
      val w = wig.toVariableStep.interSection(enh.toBedLine)
      w foreach {
        z =>
          val x = z.lines.count(q => minimum <= q._2 && q._2 <= maximum)
          val y = z.lines.length - x
          enh.gene.foreach {
            gs => gs.names.foreach {
              g =>
                if (!genes.contains(g)) {
                  genes += (g -> Array[Int](0, 0))
                }
                genes(g)(0) += x
                genes(g)(1) += y
            }
          }
      }
    }

    val b1 = biformat.bigSource(bfs.head)
    val b2 = biformat.bigSource(bfs(1))

    val bi1: EnhancerIterator = enhancerConcat(BedIterator.fromSource(b1).map(Enhancer(_)))

    val bi2 = BedIterator.fromSource(b2)

    val barry = bi1.filterWithBed(bi2).toArray

    b1.close()
    b2.close()

    def f(wf: File): Unit ={
      val ws = biformat.bigSource(wf)
      val wi = WigIterator.fromSource(ws)
      val bedi = barry.toIterator

      var w = wi.next()
      var b = bedi.next()

      while(wi.hasNext && bedi.hasNext){
        if(w.toVariableStep.hasIntersection(b.toBedLine)){addgenes(b,w)}
        if(w > b) b = bedi.next() else w = wi.next()
      }
      addgenes(b,w)

      ws.close()
    }

    if(wdir.isDirectory) wdir.listFiles.foreach(f) else f(wdir)

    val ps = new PrintStream(of)
    genes.map{
      case kv =>
        val tmp = kv._2.sum
        kv._1 -> kv._2(0).toDouble / tmp
    }.toList.sortBy{case x => x._2}.foreach{
      kv =>
        ps.println(kv._1 + "," + kv._2)
    }
    ps.close()
  }

  /**
    * sort .bed with chrommosome names, starting positions and end positions
    *
    * @param inputFile
    * @param outputFile
    */
  def bedSort(inputFile: File, outputFile: File): Unit ={
    val inputSource = biformat.bigSource(inputFile)
    val bedar = BedIterator.fromSource(inputSource).toArray
    Sorting.stableSort(bedar, (x:Block, y: Block) => x.end < y.end)
    Sorting.stableSort(bedar, (x:Block, y: Block) => x.start < y.start)
    Sorting.stableSort(bedar, (x:Block, y: Block) => Chromosome(x.chr) < Chromosome(y.chr))
    inputSource.close()
    val ps =
      if(outputFile.getName.endsWith(".gz"))
        new PrintStream(new GZIPOutputStream(new FileOutputStream(outputFile),2048))
      else
        new PrintStream(outputFile)
    bedar foreach ps.println
    ps.close()
  }

  def wigSort(inputFile: File, outputFile: File): Unit ={
    val inputSource = biformat.bigSource(inputFile)
    val wigar = WigIterator.fromSource(inputSource).toArray
    Sorting.stableSort(wigar, (x:Block, y: Block) => x.end < y.end)
    Sorting.stableSort(wigar, (x:Block, y: Block) => x.start < y.start)
    Sorting.stableSort(wigar, (x:Block, y: Block) => Chromosome(x.chr) < Chromosome(y.chr))
    inputSource.close()
    val ps =
      if(outputFile.getName.endsWith(".gz"))
        new PrintStream(new GZIPOutputStream(new FileOutputStream(outputFile),2048))
      else
        new PrintStream(outputFile)
    wigar foreach ps.println
    ps.close()
  }

  def intersectionDo[T : ClassTag,S1 <: Block,S2 <: Block](xit: Iterator[S1], yit: Iterator[S2], f: (S1, S2) => T):Array[T] = {
    val buf = ArrayBuffer[T]()
    var xunit = xit.next()
    var yunit = yit.next()
    while ((xit.hasNext || xunit.end > yunit.end) && (yit.hasNext || xunit.end <= yunit.end)) {
      if (xunit.chr == yunit.chr && xunit.start < yunit.end && xunit.end > yunit.start) {
        buf += f(xunit, yunit)
      }
      if (xunit.chr != yunit.chr || xunit.end <= yunit.end) {
        xunit = xit.next()
      }
      else{
        yunit = yit.next()
      }
    }
    val x: Array[T] = buf.toArray
    x
  }

  def enhancerConcat(xs: Iterator[Enhancer]): Iterator[Enhancer] = {
    val ys = xs.toArray
    Sorting.stableSort(ys, (x:Block, y: Block) => x.end < y.end)
    Sorting.stableSort(ys, (x:Block, y: Block) => x.start < y.start)
    Sorting.stableSort(ys, (x:Block, y: Block) => Chromosome(x.chr) < Chromosome(y.chr))
    val tmp = ys.foldLeft(Nil: List[Enhancer]){
      case (z::zs, x) =>
        if(x == z) Enhancer(x.chr, x.start, x.end, x.gene ++ z.gene, x.score) :: zs
        else x :: z :: zs
      case (Nil, x) => x :: Nil
    }
    tmp.reverse.toIterator
  }

  def tssConcat(xs: Iterator[TSS]): Iterator[TSS] = {
    val ys = xs.toArray
    Sorting.stableSort(ys, (x:Block, y: Block) => x.end < y.end)
    Sorting.stableSort(ys, (x:Block, y: Block) => x.start < y.start)
    Sorting.stableSort(ys, (x:Block, y: Block) => Chromosome(x.chr) < Chromosome(y.chr))
    val tmp = ys.foldLeft(Nil: List[TSS]){
      case (z::zs, x) =>
        if(x == z) TSS(x.chr, x.start, x.end, x.gene ++ z.gene) :: zs
        else x :: z :: zs
      case (Nil, x) => x :: Nil
    }
    tmp.reverse.toIterator
  }
}

class Chromosome private (val chr: String) extends Ordered[Chromosome] {

  override def compare(that: Chromosome): Int = {
    def chr2int(chr: String): Int = {
      try chr.toInt
      catch {
        case _: Exception => chr.head.toInt + 99
      }
    }
    chr2int(this.chr) - chr2int(that.chr)
  }

  override def toString = "chr" + chr

}

object Chromosome{

  def apply(chr: Any): Chromosome = {
    chr match {
      case x: Int => new Chromosome(x.toString)
      case x: String => new Chromosome(x.diff("chr"))
      case x: Chromosome => x
      case _ => throw new Exception
    }
  }
  object Human{
    lazy val list: List[Chromosome] =
      (1 to 22).toList.map(Chromosome(_)) ::: List("X", "Y").map(Chromosome(_))
  }
}