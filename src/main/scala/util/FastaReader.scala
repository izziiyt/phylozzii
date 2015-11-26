package util

import java.io.PrintWriter
import fdur.ModelTree
import alignment.{AminoAcid, Base}
import alignment.{Codon, CodonTable}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag
import scala.sys.process.Process

trait FastaReader {

  type Protein = Array[AminoAcid]

  type DNA = Array[Base]

  protected def cdntbl:CodonTable

  protected def fFPosition(xs: Array[Codon]): Array[Int] =
    xs.indices.collect{case i if cdntbl.is4Fold(xs(i)) => i*3 + 2}.toArray

  protected def toCodonsFromHead(dna:DNA): Array[Codon] =
    (0 until dna.length / 3).map{x => Codon(dna(x*3),dna(x*3+1),dna(x*3+2))}.toArray
}

class ExonFastaReader(val cdntbl: CodonTable) extends FastaReader{

  val swg = alignment.SWG.default

  val scorelog = "tmp/score.log.txt"

  case class Group[T](header:String, body:Array[T])

  protected def genGroup[T:ClassTag](s: Iterator[String], F: String => T): Option[Group[T]] = {
    for(l <- s) l match {
      case x if x.isEmpty => Unit
      case x if x.head == '>' =>
        val header = l.split(" ").last
        val buf = new ArrayBuffer[T]()
        for(ll <- s) ll match {
          case y if y.isEmpty => return Some(Group(header, buf.toArray))
          case y if y.head == '>' => Unit
          case _ => buf += F(ll)
        }
        return Some(Group(header, buf.toArray))
      case _ => throw new Exception
    }
    None
  }

  protected def offsetter(dna:DNA, prt:Protein, lw:PrintWriter, thresholdX:Double=0.8, thresholdY:Double=0.7):
  Option[Int] = {
    val dnas = Array(dna, dna.tail, dna.drop(2))
    val cdns = dnas.map(toCodonsFromHead)
    val scrs: Array[Double] = cdns.map(x => swg(x.map(cdntbl.transcript), prt) / 15.0)
    val (maxscr, maxoffset) = scrs.zipWithIndex.reduceLeft((n , x) => if(n._1 < x._1) x else n)
    lazy val sorted = scrs.sorted
    if (maxscr < thresholdX || sorted(2) - sorted(1) < thresholdY) None
    else {
      lw.println(scrs.mkString(","))
      Some(maxoffset)
    }
  }

  protected def genPair(nuc:Iterator[String],aa:Iterator[String]):Option[(Array[DNA],Array[Protein])] = {
    val ag = genGroup(aa, _.map(AminoAcid.fromChar).toArray)
    var ng = genGroup(nuc, _.map(Base.fromChar).toArray)
    while (true) {
      if (ng.isEmpty || ag.isEmpty) return None
      else if (ng.get.header == ag.get.header) return Some(ng.get.body, ag.get.body)
      else ng = genGroup(nuc, _.map(Base.fromChar).toArray)
    }
    None
  }

  def check(i: Int, xs: Array[DNA]): Boolean = {
    val fst = xs.find(x => ! x(i - 2).nonNuc).get(i - 2)
    val snd = xs.find(x => ! x(i - 1).nonNuc).get(i - 1)
    xs.forall(x => x(i - 2) == fst && x(i - 1) == snd)
  }

  def filtered(nucf: String,aaf: String,ouf: String, selected:Set[Int], species:Seq[String]): Unit = {
    val nucSource = Source.fromFile(nucf)
    val aaSource = Source.fromFile(aaf)
    val nucLines = nucSource.getLines()
    val aaLines = aaSource.getLines()
    val lw = new PrintWriter("log.txt")
    val w = new PrintWriter(ouf)
    try {
      val buffer = Array.fill(species.length)(new ArrayBuffer[Base]())
      var pair = genPair(nucLines, aaLines)
      w.println("##maf version=1 scoring=zero")
      while (pair.isDefined) {
        //non-filtered species
        val offset = offsetter(pair.get._1.head, pair.get._2.head, lw)
        if (offset.isDefined) {
          val dnas = pair.get._1.map(_.drop(offset.get))
          lazy val chosen = dnas.zipWithIndex.withFilter(x => selected.contains(x._2)).map(_._1)
          val n = dnas.head.length
          require(dnas.forall(x => x.length == n))
          for (i <- 2 until n by 3) {
            val fst = chosen.find(!_(i - 2).nonNuc).get(i - 2)
            val snd = chosen.find(!_(i - 1).nonNuc).get(i - 1)
            lazy val is4fold = ((fst == Base.A || fst == Base.T) && snd == Base.C) ||
              ((fst == Base.C || fst == Base.G) && snd != Base.A)
            if (!fst.nonNuc && !snd.nonNuc && is4fold &&
              chosen.forall { x => x(i - 2) == fst || x(i - 2).nonNuc } &&
              chosen.forall { x => x(i - 1) == snd || x(i - 1).nonNuc }) {
              val thds = dnas.map(_(i))
              (buffer, thds).zipped.foreach((b, x) => b += x)
            }
          }
          if (buffer.head.length >= 10000) {
            w.println("a score=0.000000")
            (buffer, species).zipped.foreach { (b, y) => w.println("s " + y + " 0 0 + 0 " + b.take(10000).mkString("")) }
            //buffer.foreach(_.clear())
            buffer.foreach(_ trimStart 10000)
          }
        }
        pair = genPair(nucLines, aaLines)
      }
      if(buffer.head.nonEmpty){
        w.println("a score=0.000000")
        (buffer, species).zipped.foreach{(b, y) => w.println("s " + y + " 0 0 + 0 " + b.mkString(""))}
        buffer.foreach{_.clear()}
      }
    }
    catch{
      case e: Throwable => println(e)
    }
    finally {
      aaSource.close()
      nucSource.close()
      lw.close()
      w.close()
    }
  }
}

object FdFilter {

  def main(args:Array[String]): Unit = {
    /* *
    * args(0): *knownNuc.fa
    * args(1): *knownAA.fa
    * args(2): "species" for selecting "4d sites". "4d sites" are defined as intersection of "4d sites" of each "species".
    * args(3): *.nh
    * args(4): codon.table.txt
    * args(5): target file
    * */
    val nucf = args(0)
    val aaf = args(1)
    val (selected, species) = file2Indices(args(2),args(3))
    val ouf = args(5)
    val cdntbl = CodonTable.fromFile(args(4))
    new ExonFastaReader(cdntbl).filtered(nucf, aaf, ouf, selected, species)
    postFilter(ouf)
  }

  def file2Indices(fi:String,nh:String): (Set[Int],Seq[String]) = {
    val sps = Source.fromFile(fi)
    try{
      val all = ModelTree.fromFile(nh).names
      val sel = sps.getLines().toSet
      val tmp = all.zipWithIndex.withFilter{case (n,_) => sel.contains(n)}.map(_._2).toSet
      (tmp, all)
    }
    catch{
      case _: Throwable => sys.error("In file2Indices.")
    }
    finally{
      sps.close()
    }
  }

  private def postFilter(fi:String):Unit = {
    val s = Source.fromFile(fi)
    val o = new PrintWriter("tmp.al")
    try {
      s.getLines().withFilter(_.replace(" ", "").map(Base.fromChar).toSet.size > 2).foreach(o.println)
    }
    catch {
      case _: Throwable => sys.error("In postFilter.")
    }
    finally {
      o.close()
      s.close()
      Process("cp tmp.al " + fi)
      Process("rm tmp.al")
    }
  }
}