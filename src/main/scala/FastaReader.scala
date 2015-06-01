import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag

trait FastaReader {

  type Protein = Array[AminoAcid]

  type myDNA = Array[Base]

  protected def cdntbl:CodonTable

  protected def isSame(cdn: Array[Codon],prt: Protein): Boolean =
    (cdn zip prt).forall{case (x,y) => cdntbl.transcript(x) == y} ||
      (cdn zip prt.tail).forall{case (x,y) => cdntbl.transcript(x) == y}

  protected def fFPosition(xs: Array[Codon]): Array[Int] =
    xs.indices.collect{case i if cdntbl.is4Fold(xs(i)) => i*3 + 2}.toArray

  protected def toCodons(dna:myDNA): Array[Codon] = {
    val n = dna.length
    require(n % 3 == 0)
    (0 until n / 3).map{x => Codon(dna(x*3),dna(x*3+1),dna(x*3+2))}.toArray
  }
}

class ExonFastaReader(val cdntbl: CodonTable) extends FastaReader{

  object MyException extends Throwable

  protected def push(x: Iterator[String],y: Iterator[String]): Option[(Array[myDNA],Array[Protein])] = {
    if(x.hasNext && y.hasNext){
      Option(read[myDNA](x,_.map(Base.fromChar).toArray),
        read[Protein](y,_.map{x => AminoAcid.fromString(x.toString)}.toArray))
    }
    else None
  }

  protected def read[T:ClassTag](s: Iterator[String],F: String => T): Array[T] = {
    val buf = new ArrayBuffer[T]()
    for(l <- s) l match{
      case x if x.isEmpty => if(buf.nonEmpty) return buf.toArray
      case x if x.head == '>' => Unit
      case _ => buf += F(l)
    }
    buf.toArray
  }

  protected def mkCandidates(dna: myDNA,prt: Protein): Array[Int] = dna.length % 3 match {
      case 0 =>
        val x = toCodons(dna)
        if(isSame(x,prt)) fFPosition(x)
        else throw MyException;Array()
      case 1 =>
        val x = toCodons(dna.tail)
        lazy val y = toCodons(dna.init)
        if(isSame(x,prt)) fFPosition(x).map(_ + 1)
        else if(isSame(y,prt)) fFPosition(y)
        else throw MyException;Array()
      case 2 =>
        val x = toCodons(dna.drop(2))
        lazy val y = toCodons(dna.dropRight(2))
        lazy val z = toCodons(dna.slice(1, dna.length - 1))
        if(isSame(x,prt)) fFPosition(x).map(_ + 2)
        else if(isSame(y,prt)) fFPosition(y)
        else if(isSame(z,prt)) fFPosition(z).map(_ + 1)
        else throw MyException;Array()
    }

  def filtered(nucf: String,aaf: String,ouf: String): Unit = {
    val nucSource = Source.fromFile(nucf)
    val aaSource = Source.fromFile(aaf)
    val nucLines = nucSource.getLines()
    val aaLines = aaSource.getLines()
    val w = new PrintWriter(ouf)
    val lw = new PrintWriter("log.txt")
    var set = push(nucLines,aaLines)
    while(set.isDefined){
      val (dnaset,prtset) = set.get
      val candidates: Array[Int] = try mkCandidates(dnaset.head,prtset.head)
      catch {
        case MyException =>
          lw.println(dnaset.head.mkString + " " + dnaset.head.length)
          lw.println(prtset.head.map(_.toWord).mkString + " " + dnaset.head.length)
          println()
          Array()
      }
      candidates.foreach(i => w.println(dnaset.map(_(i)).mkString(" ")))
      set = push(nucLines,aaLines)
    }
    aaSource.close()
    nucSource.close()
    w.close()
  }
}

object FourFoldFilter {

  def main(args:Array[String]): Unit = {
    val nucf = args(0)
    val aaf = args(1)
    val ouf = args(2)
    val cdntbl = CodonTable.fromFile(args(3))
    new ExonFastaReader(cdntbl).filtered(nucf,aaf,ouf)
  }
}