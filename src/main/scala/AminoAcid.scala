abstract class AminoAcid{
  def toWord:String
}

case object Ala extends AminoAcid{
   def toWord = "A"
}
case object Arg extends AminoAcid{
   def toWord = "R"
}
case object Asn extends AminoAcid{
   def toWord = "N"
}
case object Asp extends AminoAcid{
   def toWord = "D"
}
case object Cys extends AminoAcid{
   def toWord = "C"
}
case object Gln extends AminoAcid{
   def toWord = "Q"
}
case object Glu extends AminoAcid{
   def toWord = "E"
}
case object Gly extends AminoAcid{
   def toWord = "G"
}
case object His extends AminoAcid{
   def toWord = "H"
}
case object Ile extends AminoAcid{
   def toWord = "I"
}
case object Leu extends AminoAcid{
   def toWord = "L"
}
case object Lys extends AminoAcid{
   def toWord = "K"
}
case object Met extends AminoAcid{
   def toWord = "M"
}
case object Phe extends AminoAcid{
   def toWord = "F"
}
case object Pro extends AminoAcid{
   def toWord = "P"
}
case object Ser extends AminoAcid{
   def toWord = "S"
}
case object Thr extends AminoAcid{
   def toWord = "T"
}
case object Trp extends AminoAcid{
   def toWord = "W"
}
case object Tyr extends AminoAcid{
   def toWord = "Y"
}
case object Val extends AminoAcid{
   def toWord = "V"
}
case object Ter extends AminoAcid{
   def toWord = "Z"
}
case object Any extends AminoAcid{
  def toWord = "X"
}

object AminoAcid{
  private val values = Array(Ala,Arg,Asn,Asp,Cys,Gln,Glu,Gly,His,Ile,Leu,Lys,Met,Phe,Pro,Ser,Thr,Trp,Tyr,Val,Ter,Any)
  private val acids: Map[String,AminoAcid] = values.flatMap{case x => List(x.toString -> x,x.toWord -> x)}.toMap
  def fromString(s: String): AminoAcid = acids.get(s) match {
    case Some(x) => x
    case None => Any
  }
  val binSize = 5
}