import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag
import alignment.Base
import alignment.AminoAcid

trait FastaReader {

  type Protein = Array[AminoAcid]

  type DNA = Array[Base]

  protected def cdntbl:CodonTable

  object LocalException extends Throwable

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
      case _ => throw LocalException
    }
    None
  }

  protected def mkCandidates(dna:DNA,prt:Protein,lw:PrintWriter): Array[Int] = {
    val dnas = Array(dna,dna.tail,dna.drop(2))
    val cdns = dnas.map(x => toCodonsFromHead(x))
    val scrs = cdns.map(x => swg(x.map(cdntbl.transcript),prt))
    lw.println(scrs.map(_ / 15.0).mkString(","))
    val cdn = cdns.zipWithIndex.maxBy(x => swg(x._1.map(cdntbl.transcript),prt))
    fFPosition(cdn._1).map(_ + cdn._2)
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

  def filtered(nucf: String,aaf: String,ouf: String): Unit = {
    val nucSource = Source.fromFile(nucf)
    val aaSource = Source.fromFile(aaf)
    val nucLines = nucSource.getLines()
    val aaLines = aaSource.getLines()
    val lw = new PrintWriter("log.txt")
    val w = new PrintWriter(ouf)
    try {
      var pair = genPair(nucLines, aaLines)
      while (pair.isDefined) {
        val tmp = pair.get
        val pos = mkCandidates(tmp._1.head, tmp._2.head, lw)
        pos.foreach(i => w.println(tmp._1.map(_(i)).mkString(" ")))
        pair = genPair(nucLines, aaLines)
      }
      /*for (pair <- genPair(nucSource.getLines(), aaSource.getLines())) {
        val tmp = pair
        val pos = mkCandidates(tmp._1.head, tmp._2.head, lw)
        pos.foreach(i => w.println(tmp._1.map(_(i)).mkString(" ")))
      }*/
    }
    catch{
      case LocalException => println("heyheyhey")
    }
    finally {
      aaSource.close()
      nucSource.close()
      lw.close()
      w.close()
    }
  }
}

object FourFoldFilter {

  def main(args:Array[String]): Unit = {
    val nucf = args(0)
    val aaf = args(1)
    val ouf = args(2)
    val cdntbl = CodonTable.fromFile(args(3))
    new ExonFastaReader(cdntbl).filtered(nucf, aaf, ouf)
  }
}