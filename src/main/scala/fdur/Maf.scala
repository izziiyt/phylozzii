package fdur

import java.io._
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
import alignment.Base

object Maf {
  def readMaf(mf: String, per: Int): Array[List[Array[Base]]] = {
    val it = MafUnitIterator.fromMSA(mf)
    val totalunit = it.reduceLeft { (n, u) => n + u }
    val bases = totalunit.seqs
    val tmp = div(bases, per)
    tmp
  }

  /*def readMafv(mf: String, tree: ModelRoot): Array[List[Array[Base]]] = {
    val its = MafUnitIterator.fromMSA(mf)
    val names = tree.names
    its.map{
      it =>
        if(names.length < it.lines.length){
          sys.error("# of sequences per one unit exceeds # of tree species. ")
        }
        val n = it.length
        names.map{
          name => val tmp = it.lines.find(_.name == name)
            if(tmp.isDefined) tmp.get else Array.fill[Base](n)(Base.N)
        }
    }.toArray
  }*/

  def convert(mf: String, spmf: String, per: Int, species: Int): Unit = {
    val it = MafUnitIterator.fromMSA(mf)
    val w = new PrintWriter(spmf)
    var tmp = it.next()
    while (it.hasNext) {
      tmp = tmp + it.next()
      while (tmp.length > per) {
        val (x, y) = tmp.sliceAt(per)
        w.println(x.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
        tmp = y
      }
    }
    if (tmp.length > 0) w.println(tmp.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
    w.close()
  }

  def div(seqs: List[Array[Base]], size: Int): Array[List[Array[Base]]] = {
    @tailrec
    def f(xs: List[Array[Base]], ys: List[List[Array[Base]]], index: Int): Array[List[Array[Base]]] = {
      if (xs.head.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map { x => x.splitAt(index) }.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs, Nil, size)
  }



  /*class MafUnit(val seqs:List[Array[Base]],val length:Int){
  def +(that:MafUnit) = new MafUnit((seqs, that.seqs).zipped.map(_ ++ _), length + that.length)
  def sliceAt(n: Int) = (
    new MafUnit(seqs.map(_.take(n)), n),
    new MafUnit(seqs.map(_.drop(n)), length - n)
    )
}*/
  case class MafUnit(lines: List[MafLine]) {
    require(lines.forall(_.length == lines.head.length))

    def length = seqs.head.length

    def +(that: MafUnit) = new MafUnit((lines, that.lines).zipped.map(_ ++ _))

    def sliceAt(n: Int) = (
      new MafUnit(lines.map(_.take(n))),
      new MafUnit(lines.map(_.drop(n)))
      )

    def seqs:List[Array[Base]] = lines.map(_.seq)
  }

  /*object MafUnit {
  def apply(seqs: List[MafLine]) = {
    require(seqs.forall(_.length == seqs.head.length))
    new MafUnit(seqs.map(xs => xs.toCharArray.map(x => Base.fromChar(x))),seq.head.length)
  }
  def zero(n: Int) = new MafUnit(List.fill[Array[Base]](n)(Array()),0)
}*/

  class MafUnitIterator private(f: String, sep: String = """\p{javaWhitespace}+""") extends Iterator[MafUnit] {
    //val s = Source.fromFile(file)
    val s = Source.fromInputStream(
      new BufferedInputStream(
        if (f.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f)) else new FileInputStream(f)
      )
    )

    val lines = s.getLines()
    protected var nextOne: Option[MafUnit] = nexti()
    def hasNext = nextOne.isDefined
    def next(): MafUnit = {
      if (!hasNext) sys.error("Nothing in next.")
      else {
        val tmp = nextOne.get
        nextOne = nexti()
        tmp
      }
    }

    /*def nexti():Option[MafUnit] = {
    if(s.isEmpty) return None
    val buf = new ListBuffer[String]()
    val buf2 = new ListBuffer[Int]()
    for(line <- lines; if line != "" && !line.startsWith("#")){
      val p = line.split(sep)
      p(Int
        case "a" => if(buf.nonEmpty) return Some(MafUnit(buf.toList))
        case _ => Unit
      }
    }
    if(buf.nonEmpty && buf2.nonEmpty) Some(MafUnit(buf.toList))
    else{
      s.close()
      None
    }
  }*/

    def nexti(): Option[MafUnit] = {
      if (s.isEmpty) return None
      val buf = new ListBuffer[MafLine]()
      for (line <- lines; if line != "" && !line.startsWith("#"); p = line.split(sep)) {
        //val p = line.split(sep)
        p(0) match {
          case "s" => buf += MafLine.fromString(p)
          case "a" => if (buf.nonEmpty) {
            val tmp = Some(MafUnit(buf.toList))
            buf.clear()
            return tmp
          }
          case _ => Unit
        }
      }
      if (buf.nonEmpty) Some(MafUnit(buf.toList))
      else {
        s.close()
        None
      }
    }
  }

  case class MafLine(name: String, subname: String, start: Long, strand: String, seq: Array[Base]) {
    def length = seq.length

    /*def ++(that: MafLine) = if (name != that.name) sys.error("MafLine + operator error.")
    else MafLine(name, subname, Long.MinValue, "", seq ++ that.seq)*/
    def ++(that: MafLine) = MafLine(name, subname, Long.MinValue, "", seq ++ that.seq)
    def drop(n: Int) = MafLine(name,subname,start,strand,seq.drop(n))
    def take(n: Int) = MafLine(name,subname,start,strand,seq.take(n))
  }

  object MafLine {
    def fromString(xs: Seq[String]) = {
      val names = xs(1).split('.')
      //if(names.length == 2)
      MafLine(names(0), if (names.length == 2) names(1) else "",
        xs(2).toLong, xs(4), xs(6).toCharArray.map(Base.fromChar))
    }
  }

  object MafUnitIterator {
    def fromMSA(file: String, sep: String = """\p{javaWhitespace}+""") = new MafUnitIterator(file, sep)
  }

}