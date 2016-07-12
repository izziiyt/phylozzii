package phylozzii.core

import biformat.BedIterator.BedLine

sealed trait SpecBedLine extends biformat.Block{
  def merge(that: SpecBedLine): SpecBedLine
  def length = end - start
  def gene: Array[Genes]
  def toBedLine = BedLine(chr,start,end)
}

trait SpecBedLineTrait{
  val TSSLength = 1001
  val EnhancerLength = 401
}

case class Enhancer(chr: String, start: Int, end: Int, gene: Array[Genes], score: Double) extends SpecBedLine {
  def merge(that: SpecBedLine): SpecBedLine = that match {
    case Enhancer(_, _, _, _gene, _) => Enhancer(chr, start, end, this.gene ++ _gene, this.score)
    case _ => throw new UnsupportedOperationException
  }
  def appendableWith(that: biformat.Block) = that match {
    case Enhancer(_chr, _start, _, _, _) => chr == _chr && start == _start
    case _ => false
  }
  def compare(that: Enhancer): Int = {
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
  override def equals(that: Any): Boolean = that match {
    case Enhancer(_chr, _start, _end,_,_) =>
      _chr == chr && _start == start
    case _ =>
      false
  }
}

object Enhancer extends SpecBedLineTrait{
  def apply(line: BedLine): Enhancer = {
    require(line.blockCount == 2)
    val start = line.start + (if(line.blockSize.head == EnhancerLength) line.blockStarts.head else line.blockStarts.last)
    new Enhancer(line.chr, start, start + EnhancerLength, Array(Genes(line.name)), 0.0)
  }

  /*def apply(str: String): Enhancer = {
    val args = str.split("""\p{javaWhitespace}""")
    val genes = args(3).split("~").map(Genes(_))
    new Enhancer(args.head, args(1).toInt, args(2).toInt, args(3).split("~").map(Genes(_)), args(4).toDouble)
  }*/
}

case class TSS(chr: String, start: Int, end: Int, gene: Array[Genes]) extends SpecBedLine{
  def merge(that: SpecBedLine): SpecBedLine = that match {
    case TSS(_, _, _, _gene) => TSS(chr, start, end, this.gene ++ _gene)
    case _ => throw new UnsupportedOperationException
  }
  def appendableWith(that: biformat.Block) = that match {
    case TSS(_chr, _start, _, _) => chr == _chr && start == _start
    case _ => false
  }
  def compare(that: TSS): Int = {
    def chr2int(str: String): Int = {
      val suffix = str.diff("chr")
      try suffix.toInt catch {case _:Exception => suffix.head.toInt + 99}
    }
    val tmp = chr2int(this.chr) - chr2int(that.chr)
    if(tmp == 0) this.start - that.start
    else tmp
  }
  def withScore(x: Double): Enhancer = Enhancer(chr, start, end, gene, x)
  override def toString: String = s"$chr\t$start\t$end\t" + gene.mkString("~")
  override def equals(that: Any): Boolean = that match {
    case TSS(_chr, _start, _end,_) =>
      _chr == chr && _start == start
    case _ =>
      false
  }
}

object TSS extends SpecBedLineTrait{
  def apply(line: BedLine): TSS = {
    require(line.blockCount == 2)
    val start = line.start + (if(line.blockSize.head == TSSLength) line.blockStarts.head else line.blockStarts.last)
    new TSS(line.chr, start, start + TSSLength, Array(Genes(line.name)))
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

