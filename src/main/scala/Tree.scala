import breeze.linalg.{DenseVector,DenseMatrix}
import math.exp
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Tree{

  def cont:Content
  def format():Tree
  def setPosterior(likelihood:Double,m:EvolutionModel):Unit
  def setAlignment(x:List[DNA]):List[DNA]
  def setBranch(x:List[Double]):List[Double]
  def collectF(m:EvolutionModel):List[DenseVector[Double]]
  def collectN(m:EvolutionModel):List[DenseMatrix[Double]]
  def collectT:List[Double]
}

case class Node(left:Tree,right:Tree,cont:ContentOfNode) extends Tree{

  def setAlignment(x:List[DNA]) = {
    val tmp = left.setAlignment(x)
    right.setAlignment(tmp)
  }

  def format() = {
    cont.format()
    right.format()
    left.format()
    this
  }

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
    right.setPosterior(likelihood,m)
    left.setPosterior(likelihood,m)
  }
}

case class Leaf(species:String,cont:ContentOfLeaf) extends Tree{

  def setBranch(x:List[Double]) = {
    cont.t_=(x.head)
    x.tail
  }

  def setAlignment(x:List[DNA]) = {
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
    {case (left,right) => Node(left,right,ContentOfNode(0.0))}

  def node: Parser[Tree] = nodePair~":"~value ^^
    {case (left,right)~":"~value => Node(left,right,ContentOfNode(value.toDouble))} | leaf

  def leaf: Parser[Leaf] = name~":"~value ^^
    {case name~":"~value => Leaf(name,ContentOfLeaf(value.toDouble,DNA.N))}

  def nodePair: Parser[(Tree,Tree)] = "("~>node~","~node<~")"  ^^
    {case left~","~right => (left,right)}

  def value: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def name: Parser[String] = ident
}

case class Count(Fd:List[DenseVector[Double]],Ns:List[DenseMatrix[Double]],
                 T:List[Double],ns:DenseVector[Double],likelihood:Double)