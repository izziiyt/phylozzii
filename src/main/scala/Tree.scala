import breeze.linalg.{DenseVector,DenseMatrix}
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Tree{
  def cont:Content
  def isNull:Boolean
  def format():Tree
  def setColumn(x:Array[Char]):Array[Char]
  def setBranch(x:List[Double]):List[Double]
  def setTransition(m:EvolutionModel)
  def setPosterior(l:Double)
  def branches:List[Double]
  def names:List[String]
  override def toString:String
  def setPosteriorNull(l:Double,b:DenseVector[Double])
}

case class Node(left:Tree,right:Tree,cont:Content) extends Tree{

  def setColumn(x:Array[Char]) = {
    val tmp = left.setColumn(x)
    right.setColumn(tmp)
  }

  def isNull = cont.isNull

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

  def collectn(m:EvolutionModel):DenseVector[Double] = (cont.alpha :* m.pi) / likelihood(m)

  def setBranch(x:List[Double]) = {
    val y = left.setBranch(x)
    val z = right.setBranch(y)
    cont.t_=(z.head)
    z.tail
  }

  def setPosterior(likelihood:Double){
    if(isNull) setPosteriorNull(likelihood,cont.beta)
    else{
      cont.setPosterior(likelihood)
      left.setPosterior(likelihood)
      right.setPosterior(likelihood)
    }
  }

  def setPosteriorNull(likelihood:Double,beta:DenseVector[Double]){
    cont.setPosteriorNull(likelihood,beta)
    left.setPosteriorNull(likelihood,beta)
    right.setPosteriorNull(likelihood,beta)
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

  def setPosteriorNull(likelihood:Double,beta:DenseVector[Double]){cont.setPosteriorNull(likelihood,beta)}

  def isNull = cont.isNull

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

  def setPosterior(likelihood:Double){
    cont.setPosterior(likelihood)
  }

  def setTransition(m:EvolutionModel){
    cont.setTransProb(m)
  }

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

