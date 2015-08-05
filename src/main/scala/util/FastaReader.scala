package util

import java.io.PrintWriter

import alignment.{AminoAcid, Base}
import alignment.{Codon, CodonTable}
import fdur.FdurTree
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

  val scorelog = "score.log.txt"

  case class Group[T](header:String,body:Array[T])

  protected def genGroup[T:ClassTag](s: Iterator[String],F: String => T): Option[Group[T]] = {
    for(l <- s) l match {
      case x if x.isEmpty => Unit
      case x if x.head == '>' =>
        val header = l.split(" ").last
        val buf = new ArrayBuffer[T]()
        for(ll <- s) ll match {
          case y if y.isEmpty => return Some(Group(header,buf.toArray))
          case y if y.head == '>' => Unit
          case _ => buf += F(ll)
        }
        return Some(Group(header,buf.toArray))
      case _ => throw new Exception
    }
    None
  }

  protected def mkCandidates(dna:DNA,prt:Protein,lw:PrintWriter,threshold:Double): Array[Int] = {
    val dnas = Array(dna, dna.tail, dna.drop(2))
    val cdns = dnas.map(toCodonsFromHead)
    val scrs:Array[Double] = cdns.map(x => swg(x.map(cdntbl.transcript), prt) / 15.0).sorted
    if (scrs.max < threshold || scrs(2) - scrs(1) < threshold) Array()
    else {
      lw.println(scrs.mkString(","))
      val cdn = cdns.zipWithIndex.maxBy(x => swg(x._1.map(cdntbl.transcript), prt))
      fFPosition(cdn._1).map(_ + cdn._2)
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

  def filtered(nucf: String,aaf: String,ouf: String, indices:Set[Int]): Unit = {
    val nucSource = Source.fromFile(nucf)
    val aaSource = Source.fromFile(aaf)
    val nucLines = nucSource.getLines()
    val aaLines = aaSource.getLines()
    val lw = new PrintWriter("log.txt")
    val w = new PrintWriter(ouf)
    try {
      var pair = genPair(nucLines, aaLines)
      while (pair.isDefined) {
        //not filtered species
        val all = pair.get.zipped.toList
        //filtered by selected species
        val sel = pair.get.zipped.toList.zipWithIndex.withFilter{case (_,i) => indices.contains(i)}.map(_._1)
        val n = all.head._1.length
        //4d sites of selected species
        val poss:Seq[Set[Int]] = sel.map{case (ds,ps) => mkCandidates(ds, ps, lw, 0.8).toSet}
        //catch intersection of selected species's 4d sites
        poss.foldLeft((0 until n).toSet)(_ & _).foreach(i => w.println(all.map(_._1(i)).mkString(" ")))
        pair = genPair(nucLines, aaLines)
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
    * args(2): "species" for selecting "4d sites". "4d sites" are defined as intersection of "4d sites" of "species".
    * args(3): *.nh
    * args(4): codon.table.txt
    * args(5): target file
    * */
    val nucf = args(0)
    val aaf = args(1)
    val indices = file2Indices(args(2),args(3))
    val ouf = args(5)
    val cdntbl = CodonTable.fromFile(args(4))
    new ExonFastaReader(cdntbl).filtered(nucf, aaf, ouf, indices)
    postFilter(ouf)
  }

  def file2Indices(fi:String,nh:String): Set[Int] = {
    val sps = Source.fromFile(fi)
    try{
      val all = FdurTree.fromFile(nh).names
      val sel = sps.getLines().toSet
      all.zipWithIndex.withFilter{case (n,i) => sel.contains(n)}.map(_._2).toSet
    }
    catch{
      case _: Throwable => sys.error("In file2Indices")
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
      case _: Throwable => sys.error("In postFilter routine")
    }
    finally {
      o.close()
      s.close()
      Process("cp tmp.al " + fi)
      Process("rm tmp.al")
    }
  }
}