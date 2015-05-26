import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag

trait FastaReader {

  type Protein = Array[AminoAcid]
  type myDNA = Array[Base]

  protected def cdntbl:CodonTable

  protected def read[T:ClassTag](inf: String,F: String => T): Array[Array[T]] = {
    val lclBuf = new ArrayBuffer[T]()
    val glblBuf  = new ArrayBuffer[Array[T]]()
    for(l <- Source.fromFile(inf).getLines()){
      l match{
        case x if x.head == '>' => None
        case x if x.isEmpty => glblBuf += lclBuf.toArray;lclBuf.clear()
        case _ => lclBuf += F(l)
      }
    }
    glblBuf += lclBuf.toArray
    glblBuf.toArray
  }

  protected def readNuc(inf: String) = read[myDNA](inf,_.map(Base.fromChar).toArray)

  protected def readAA(inf: String) = read[Protein](inf,_.map{x => AminoAcid.fromString(x.toString)}.toArray)

  def filter(nucf: String,aaf: String,ouf: String) = {
    val dnasets = readNuc(nucf)
    val prtsets = readAA(aaf)
    val w = new PrintWriter(ouf)
    for((dnaset,prtset) <- dnasets zip prtsets){
      val hgdna = dnaset.head
      val hgprt = prtset.head
      val candidates: Array[Int] = hgdna.length % 3 match {
        case 0 =>
          val x = toCodons(hgdna)
          if(isSame(x,hgprt)) fFPosition(x)
          else                Array()
        case 1 =>
          val x =      toCodons(hgdna.tail)
          lazy val y = toCodons(hgdna.init)
          if(isSame(x,hgprt))      fFPosition(x).map(_ + 1)
          else if(isSame(y,hgprt)) fFPosition(y)
          else                     Array()
        case 2 =>
          val x =      toCodons(hgdna.drop(2))
          lazy val y = toCodons(hgdna.dropRight(2))
          lazy val z = toCodons(hgdna.slice(1, hgdna.length - 1))
          if(isSame(x,hgprt))      fFPosition(x).map(_ + 2)
          else if(isSame(y,hgprt)) fFPosition(y)
          else if(isSame(z,hgprt)) fFPosition(z).map(_ + 1)
          else                     Array()
      }
      // finds 4 fold degenerative sites
      candidates.foreach(i => w.println(dnaset.map(_(i)).mkString))
    }
    w.close()
  }

  protected def isSame(cdn: Array[Codon],prt: Protein): Boolean =
    (cdn zip prt).forall{case (x,y) => cdntbl.transcript(x) == y} ||
      (cdn zip prt.tail).forall{case (x,y) => cdntbl.transcript(x) == y}

  protected def fFPosition(xs: Array[Codon]): Array[Int] =
    (0 until xs.length).collect{case i if cdntbl.is4Fold(xs(i)) => i*3 + 2}.toArray

  protected def toCodons(dna:myDNA): Array[Codon] = {
    val n = dna.length
    require(n % 3 == 0)
    (0 until n / 3).map{x => Codon(dna(x),dna(x+1),dna(x+2))}.toArray
  }

  def complete4foldFilter: String => Unit
  def hg19Only4foldFilter: String => Unit

}
