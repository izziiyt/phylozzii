package phylozzii.core

import java.io._
import java.util.zip.GZIPOutputStream

import biformat.Block
import alignment.Base
import biformat.WigIterator.VariableStep
import biformat.{BedIterator, WigIterator}
import biformat.BedIterator.BedLine
import breeze.linalg._
import breeze.numerics._
import breeze.plot._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Sorting

object Others extends {
  /*def wighist(wigs: Source, beds: Option[Source], chr: String, out: PrintStream = System.out) {
    val bedit = beds map (b => BedIterator.fromSource(b).filter(_.chr == chr))
    val preWigit = WigIterator.fromSource(wigs, 2048)
    val wigit = bedit map preWigit.filterWithBed getOrElse preWigit
    out.println(wigit.hist(1000, 1).mkString(","))
  }*/

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

  /**
    * take wig lines randomly
    *
    * @param number
    * @param wigs
    * @param out
    */
  /*def randomwig(number: Int, wigs: Source, out: PrintStream = System.out): Unit ={
    val MAX = wigs.getLines.length - 1
    wigs.reset()
    val r = scala.util.Random
    val indices = (0 until number).map{_ => r.nextInt(MAX)}.sortWith(_ > _)
    val stairs = indices.foldLeft(Nil:List[Int]){
      case (z::zs,x) => x :: z - x - 1 :: zs
      case (Nil,x) => x :: Nil
    }

    val wigit = WigIterator.fromSource(wigs)
    var wig = wigit.next()

    for(i <- 1 to number) yield {
      val j = i * 100 / number
      wig = if(stairs(i - 1) == -1) wig else wigit.drop(stairs(i - 1)).next()
      val diff = wig.length - Enhancer.EnhancerLength
      if(diff >= 0){
        val n = if(diff == 0) 0 else r.nextInt(diff)
        val tmpwig = wig.toVariableStep.lines.slice(n, n+Enhancer.EnhancerLength)
        out.println(VariableStep(wig.chr,wig.span,tmpwig))
      }
      /*if(i % 20 == 0 || i == number - 1) {
        printf("%3d%% [%-50s]\r", j, "=" * (j / 2))
        System.out.flush()
      }*/
    }
    println()
  }*/

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

  /**
    *
    * @param wf
    * @param bf
    * @param out
    */
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

  /* def bedChrSplit(bedSource: Source, odir: File = new File(".")): Unit = {
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
   }*/

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

  /*def wigwigphyloP(ws1: Source, ws2: Source, prnt: PrintStream): Unit = {
    val SIZE = 100
    val mat = DenseMatrix.zeros[Int](SIZE, SIZE)

    def save(first: VariableStep, second: VariableStep): Unit = {
      val start = max(first.start, second.start)
      val end = min(first.end, second.end)
      val x1 = first.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      val x2 = second.lines.dropWhile(_._1 < start).takeWhile(_._1 < end)
      require(x1.length == x2.length)
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
  }*/

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
            mat(min(99, (x._2 * SIZE).toInt), min(99, (y2 * SIZE).toInt)) += 1}
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

  /**
    * make .hist with .bed .wig
    *
    * @param gotsvf
    * @param bed
    * @param wigf
    */
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

  /**
    * prints significant genes on an output stream
    *
    * @param inputSources
    * @param ps
    * @param minimum
    * @param maximum
    */
  /*def geneList(inputSources: Array[Source], ps: PrintStream, minimum: Double, maximum: Double) = {
    val names = scala.collection.mutable.Set.empty[String]
    inputSources.foreach{
      _.getLines().
        map(Enhancer(_)).
        dropWhile(_.score < minimum).
        takeWhile(_.score < maximum).
        foreach(_.gene.foreach(_.names.foreach(names += _)))
    }
    names.toArray.foreach(ps.println)
  }*/

  /*def enhancers(inputSource: Source, referenceSource: Source, ps: PrintStream) = {
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
      if (eblsunit.chr != enunit.chr || enunit.end <= eblsunit.start) {
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
  }*/

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

  /**
    * extracts enhancer regions from .wig file, the enhancer regions are defined in eblsSource
    * extracts regions are (start - WING) to (end + WING)
    *
    * @param inputSource .wig file
    * @param eblsSource enhancer regions are defined in
    * @param ps output stream
    */
  /*def extractEnhancers(inputSource: Source, eblsSource: Source, ps: PrintStream): Unit = {
    val WING = 400
    val enIt = BedIterator.fromSource(inputSource).map{
      b =>
        val tmp = Enhancer(b)
        Enhancer(tmp.chr, max(tmp.start - WING, 0), tmp.end + WING,tmp.gene, tmp.score)
    }.toArray.sorted.toIterator
    val eblsIt = WigIterator.fromSource(eblsSource)
    val enhArray = intersectionDo(enIt, eblsIt,
      (en: Enhancer, ebls: WigIterator.WigUnit) => {
        val lines = ebls.toVariableStep.lines.filter{case (i,_) => i >= math.max(en.start, ebls.start) && i < math.min(en.end, ebls.end)}
        VariableStep(en.chr, 1, lines)
      })
    val result = enhArray.tail.foldLeft(Array(enhArray.head)){
      case (zs, x) =>
        if(zs.last.end == x.start) zs.init :+ (zs.last + x).toVariableStep
        else zs :+ x
    }
    result.foreach(ps.println)
    //result.foreach( t => assert(t.length == 401))
  }*/

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