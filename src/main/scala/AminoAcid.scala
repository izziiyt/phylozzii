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
case object End extends AminoAcid{
   def toWord = "*"
}

object AminoAcid{
  val fromInt:Array[AminoAcid] = Array(Ala,Arg,Asn,Asp,Cys,Gln,Glu,Gly,His,Ile,Leu,Lys,Met,Phe,Pro,Ser,Thr,Trp,Tyr,Val,End)
  val toInt:AminoAcid => Int = Map(
    Ala -> 0,Arg -> 1,Asn -> 2,Asp -> 3,Cys -> 4,
    Gln -> 5,Glu -> 6,Gly -> 7,His -> 8,Ile -> 9,
    Leu -> 10,Lys -> 11,Met -> 12,Phe -> 13,Pro -> 14,
    Ser -> 15,Thr -> 16,Trp -> 17,Tyr -> 18,Val -> 19,End -> 20)
  val fromString:String => AminoAcid = fromInt.flatMap{case x => List(x.toString -> x,x.toWord -> x)}.toMap
  val binSize = 5
  def toWord:String = ""
}