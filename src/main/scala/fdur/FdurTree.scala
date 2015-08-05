package fdur

import java.io.FileReader
import alignment.Base
import breeze.linalg.DenseVector

trait FdurTree extends Tree{
  def setPosteriorNull(l:Double,b:DenseVector[Double])
  def setTransition(x:EvolutionModel):Unit
  def setPosterior(l:Double)
  def names:List[String]
  def format():this.type
  def branches:List[Double]
  def setBranch(x:List[Double]):List[Double]
  def isNull = cont.isNull
  def cont:Content
  def setColumn(x:Array[Base]):Array[Base]
}

object FdurTree extends NHParser4Fdur{
  @deprecated
  def apply(nhFile:String):FdurNode = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromFile(nhFile:String):FdurNode = {
    val reader = new FileReader(nhFile)
    parseAll(tree,reader).get
  }
  def fromString(nhString:String) = {
    parseAll(tree,nhString).get
  }
}

class NHParser4Fdur extends NHParser[FdurTree] {

  def tree:Parser[FdurNode] =  nodePair<~";"  ^^
    {case (left,right) => FdurNode(left,right,ContentOfRoot())}

  def node:Parser[FdurTree] = nodePair~":"~value ^^
    {case (left,right)~":"~value => FdurNode(left,right,ContentOfNode(value.toDouble))} | leaf

  def leaf:Parser[FdurLeaf] = name~":"~value ^^
    {case name~":"~value => FdurLeaf(name,ContentOfLeaf(value.toDouble,Base.N))}

  //def nodePair:Parser[(FdurTree,FdurTree)] = "("~>node~","~node<~")"  ^^
  //{case left~","~right => (left,right)}
}
