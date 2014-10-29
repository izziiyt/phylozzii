import breeze.linalg.{DenseVector,DenseMatrix}
import math.exp
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Tree{
  def cont:Content
  def format():Tree
  def setAlignment(x:List[Char]):List[Char]
  def setBranch(x:List[Double]):List[Double]
  def setTransition(m:EvolutionModel)
  def setPosterior(l:Double,m:EvolutionModel)
  def collectF(m:EvolutionModel):List[DenseVector[Double]]
  def collectN(m:EvolutionModel):List[DenseMatrix[Double]]
  def collectT:List[Double]
  def branches:List[Double]
  def names:List[String]
}

case class Node(left:Tree,right:Tree,cont:Content) extends Tree{

  def setAlignment(x:List[Char]) = {
    val tmp = left.setAlignment(x)
    right.setAlignment(tmp)
  }

  def format() = {
    cont.format()
    right.format()
    left.format()
    this
  }

  def names = left.names ::: right.names

  def branches = left.branches ::: right.branches ::: List(cont.t)

  def likelihood(m:EvolutionModel):Double = cont.likelihood(m)

  def collectF(m:EvolutionModel) = left.collectF(m) ::: right.collectF(m) ::: List(cont.FdVec(m))

  def collectN(m:EvolutionModel) = left.collectN(m) ::: right.collectN(m) ::: List(cont.NsMat(m))

  def collectT = left.collectT ::: right.collectT ::: List(cont.t)

  def collectn(m:EvolutionModel):DenseVector[Double] = (cont.alpha :* m.pi) / likelihood(m)

  def count(m:EvolutionModel) = Count(left.collectF(m) ::: right.collectF(m),
    left.collectN(m) ::: right.collectN(m),left.collectT ::: right.collectT,collectn(m),likelihood(m))

  def setBranch(x:List[Double]) = {
    val y = left.setBranch(x)
    val z = right.setBranch(y)
    cont.t_=(z.head)
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

  def branches = List(cont.t)

  def names = List(species)

  def setAlignment(x:List[Char]) = {
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

  def collectF(m:EvolutionModel) = List(cont.FdVec(m))

  def collectN(m:EvolutionModel) = List(cont.NsMat(m))

  def collectT = List(cont.t)
}

object Tree extends NHParser{

  def apply(nhFile:String):Node = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
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

case class Count(Fd:List[DenseVector[Double]],Ns:List[DenseMatrix[Double]],
                 T:List[Double],ns:DenseVector[Double],likelihood:Double)