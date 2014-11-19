import java.io.PrintWriter

object Maf2Alignments{
  def main(args:Array[String]){
    exe(args(0),args(1),args(2),args(3).toInt)
  }

  private def exe(maf:String,nh:String,outDir:String,perSize:Int){
    val mafunits = FilteredMafParser(maf).grouped(perSize)
    val names = Tree(nh).names
    val map = (names,0 until names.length).zipped.map(_ -> _).toMap
    val buf = Array.fill(names.length)(4)
    var c = 0
    for(units <- mafunits){
      val out = new PrintWriter(outDir + "/" + c)
      for(unit <- units){
        for(i <- 0 until unit.seqs(0).length){
          for(seq <- unit.seqs){
            buf(map(seq.chr.name)) = trans(seq.seq(i))
          }
          buf.foreach(b => out.print(b + " "))
          out.println()
          (0 until buf.length).foreach(buf(_) = 4)
        }
      }
      out.close()
      c += 1
    }
  }
  private def trans(x:Char):Int = {
    x match {
      case 'a' | 'A' => 0
      case 'c' | 'C' => 1
      case 'g' | 'G' => 2
      case 't' | 'T' => 3
      case _ => 4
    }
  }
}
