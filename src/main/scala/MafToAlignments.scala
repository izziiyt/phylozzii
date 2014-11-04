import java.io.PrintWriter

object MafToAlignments {

  def apply(maf:String,nh:String,output:String){
    val mafunits = FilteredMafParser(maf)
    val names = Tree(nh).names
    val mymap = (names,0 until names.length).zipped.map(_ -> _).toMap
    val out = new PrintWriter(output)
    val buf = new Array[Int](names.length)
    for(i <- 0 until buf.length) buf(i) = 4
    for(unit <- mafunits){
      for(i <- 0 until unit.seqs(0).length){
        for(seq <- unit.seqs){
          buf(mymap(seq.chr.name)) = trans(seq.seq(i))
        }
        if(!isGerbage(buf)){buf.foreach(b => out.print(b + " "));out.println()}
      }
    }
    out.close()
  }

  private def isGerbage(xs:Array[Int]) = {
    val tmp = xs.foldLeft(0)((x,i) => if(i == 4) x + 1 else x)
    if(tmp >= xs.length - 1) true else false
  }

  private def trans(x:Char):Char = {
    x match {
      case 'a' | 'A' => 0.toChar
      case 'c' | 'C' => 1.toChar
      case 'g' | 'G' => 2.toChar
      case 't' | 'T' => 3.toChar
      case _ => 4.toChar
    }
  }
}