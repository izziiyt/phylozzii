package phylozzii.core

import biformat.BedIterator.BedLine

case class Enhancer(chr: String, start: Int, end: Int, gene: Array[Genes], score: Double) extends Ordered[Enhancer] with biformat.Block {
  def merge(that: Enhancer): Enhancer = Enhancer(chr, start, end, this.gene ++ that.gene, this.score + that.score)
  def length = end - start
  def appendableWith(that: biformat.Block) = that match {
    case Enhancer(_chr, _start, _, _, _) => chr == _chr && end + 1 == _start
    case _ => false
  }
  override def compare(that: Enhancer): Int = {
    def chr2int(str: String): Int = {
      val suffix = str.diff("chr")
      try suffix.toInt catch {case _:Exception => suffix.head.toInt + 99}
    }
    val tmp = chr2int(this.chr) - chr2int(that.chr)
    if(tmp == 0) this.start - that.start
    else tmp
  }
  def toBedLine = BedLine(chr,start,end)
  def withScore(x: Double): Enhancer = Enhancer(chr, start, end, gene, x)
  override def toString: String = s"$chr\t$start\t$end\t" + gene.mkString("~") + s"\t$score"
}

object Enhancer{
  val TSSLength = 1001
  val EnhancerLength = 401
  def apply(line: BedLine): Enhancer = {
    require(line.blockCount == 2)
    val start = line.start + (if(line.blockSize.head == EnhancerLength) line.blockStarts.head else line.blockStarts.last)
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

