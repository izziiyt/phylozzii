import breeze.linalg.{DenseVector,DenseMatrix}
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

sealed trait Tree{
  def cont:Content
  def format():Tree
  def setColumn(x:Array[Char]):Array[Char]
  def setBranch(x:List[Double]):List[Double]
  def setTransition(m:EvolutionModel)
  def setPosterior(l:Double,m:EvolutionModel)
  def collectF(m:EvolutionModel):ListBuffer[DenseVector[Double]]
  def collectN(m:EvolutionModel):ListBuffer[DenseMatrix[Double]]
  def branches:List[Double]
  def names:List[String]
  override def toString:String


}

case class Node(left:Tree,right:Tree,cont:Content) extends Tree{

  def setColumn(x:Array[Char]) = {
    val tmp = left.setColumn(x)
    right.setColumn(tmp)
  }

  def format() = {
    cont.format()
    right.format()
    left.format()
    this
  }

  override def toString = cont match {
    case ContentOfRoot(_) => "(" + left + "," + right + ");"
    case ContentOfNode(_) => "(" + left + "," + right + "):" + cont.t
  }

  def names = left.names ::: right.names

  def branches = left.branches ::: right.branches ::: List(cont.t)

  def likelihood(m:EvolutionModel):Double = cont.likelihood(m)

  def collectF(m:EvolutionModel):ListBuffer[DenseVector[Double]]
  = left.collectF(m) ++ right.collectF(m) += cont.FdVec(m)

  def collectN(m:EvolutionModel):ListBuffer[DenseMatrix[Double]]
  = left.collectN(m) ++ right.collectN(m) += cont.NsMat(m)



  def collectn(m:EvolutionModel):DenseVector[Double] = (cont.alpha :* m.pi) / likelihood(m)

  def setBranch(x:List[Double]) = {
    val y = left.setBranch(x)
    val z = right.setBranch(y)
    if(!z.isEmpty) cont.t_=(z.head)
    z.tail
  }

  def setPosterior(likelihood:Double,m:EvolutionModel){
    cont.setPosterior(likelihood,m)
    left.setPosterior(likelihood,m)
    right.setPosterior(likelihood,m)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
    left.setTransition(m)
    right.setTransition(m)
  }
}

case class Leaf(species:String,cont:ContentOfLeaf) extends Tree{

  def setBranch(x:List[Double]) = {
    cont.t_=(x.head)
    x.tail
  }

  override def toString = species + ":" + cont.t

  def branches = List(cont.t)

  def names = List(species)

  def setColumn(x:Array[Char]) = {
    cont.nuc_=(x.head)
    x.tail
  }

  def format() = {
    cont.format()
    this
  }

  def setPosterior(likelihood:Double,m:EvolutionModel){
    cont.setPosterior(likelihood,m)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
  }

  def collectF(m:EvolutionModel) = ListBuffer[DenseVector[Double]](cont.FdVec(m))

  def collectN(m:EvolutionModel) = ListBuffer(cont.NsMat(m))
}

object Tree extends NHParser{
  @deprecated
  def apply(nhFile:String):Node = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromFile(nhFile:String):Node = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromString(nhString:String) = {
    parseAll(tree,nhString).get
  }
}

class NHParser extends JavaTokenParsers {

  def tree: Parser[Node] =  nodePair<~";"  ^^
    {case (left,right) => Node(left,right,ContentOfRoot())}

  def node: Parser[Tree] = nodePair~":"~value ^^
    {case (left,right)~":"~value => Node(left,right,ContentOfNode(value.toDouble))} | leaf

  def leaf: Parser[Leaf] = name~":"~value ^^
    {case name~":"~value => Leaf(name,ContentOfLeaf(value.toDouble,4))}

  def nodePair: Parser[(Tree,Tree)] = "("~>node~","~node<~")"  ^^
    {case left~","~right => (left,right)}

  def value: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def name: Parser[String] = ident
}

